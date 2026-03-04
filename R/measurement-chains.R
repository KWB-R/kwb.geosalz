#' Measurement Chains: Get Metadata
#'
#' @param file path to measurement chains metadata file. Default:
#' kwb.geosalz:::extdata_file("metadata_messketten.csv")
#' @return tibble with measurement chains metadata
#' @export
#' @importFrom readr cols col_character col_integer col_double read_csv
#' @examples
#' mc_metadata <- kwb.geosalz::get_measurementchains_metadata()
#' str(mc_metadata)
#' mc_metadata
get_measurementchains_metadata <- function(
    file = extdata_file("metadata_messketten.csv")
)
{
  chr <- readr::col_character()
  int <- readr::col_integer()
  dbl <- readr::col_double()
  
  col_types <- readr::cols(
    galerie = chr,
    brunnen_nummer = int,
    dn = int,
    einbau_pumpe = chr,
    einbau_messkette = chr,
    filteroberkante_muGOK = dbl,
    filterunterkante_muGOK = dbl,
    sensor_id = int,
    sensor_endnummer = int,
    einbau_sensor_muGOK = dbl
  )
  
  readr::read_csv(file, col_types = col_types)
}

#' Measurement Chains: Create SFTP credentials (curl backend)
#'
#' @return list with server/username/password/port
#' @export
create_sftp_connection <- function()
{
  con <- get_environment_variables(
    server   = "MESSKETTEN_SERVER",
    username = "MESSKETTEN_USER",
    password = "MESSKETTEN_PASSWORD",
    check. = TRUE
  )
  
  port <- Sys.getenv("MESSKETTEN_PORT", unset = "22")
  port <- suppressWarnings(as.integer(port))
  if (is.na(port) || port <= 0L) port <- 22L
  
  c(con, list(port = port))
}


#' Measurement Chains: Get Available Files from SFTP
#'
#' Lists all available measurement chain files on the SFTP server and extracts
#' metadata encoded in the file names. The function recursively scans the
#' remote directory, identifies files belonging to measurement chains and
#' returns a tidy table with parsed metadata.
#'
#' Files that do not match the expected measurement chain naming pattern are
#' automatically ignored and reported in an attribute of the returned object.
#'
#' Expected filename pattern (without extension):
#'
#' \code{LF_<sensor_id><sensor_endnummer>-YYYY-MM-DD-HHMM}
#'
#' or
#'
#' \code{<sensor_id><sensor_endnummer>-YYYY-MM-DD-HHMM}
#'
#' @param sftp_connection connection credentials as returned by
#'   \code{\link{create_sftp_connection}}.
#'
#' @param debug logical. If \code{TRUE}, prints diagnostic messages about files
#'   that do not match the measurement chain naming scheme. Default: \code{FALSE}.
#'
#' @return A tibble with metadata for all detected measurement chain files.
#' The table includes:
#'
#' \itemize{
#' \item \code{sftp_path} – full path of the file on the SFTP server
#' \item \code{galerie} – gallery identifier derived from folder name
#' \item \code{brunnen_nummer} – well number derived from folder name
#' \item \code{prefix} – optional file prefix (e.g. \code{LF_})
#' \item \code{sensor_id} – numeric sensor identifier
#' \item \code{sensor_endnummer} – sensor end number
#' \item \code{datum_uhrzeit} – timestamp parsed from the filename
#' }
#'
#' Additionally the returned object contains an attribute:
#'
#' \itemize{
#' \item \code{ignored_files} – tibble listing files that were skipped because
#'   their names do not match the expected measurement chain pattern.
#' }
#'
#' @details
#' The function performs the following steps:
#'
#' \enumerate{
#' \item Recursively lists all files in the configured SFTP directory
#' \item Splits paths into folder and filename components
#' \item Validates filenames against the measurement chain naming scheme
#' \item Parses sensor metadata and timestamps from filenames
#' \item Returns a tidy table with extracted metadata
#' }
#'
#' This approach makes the import robust against unrelated files (e.g.
#' \code{Thumbs.db}, \code{desktop.ini}, or documentation files) that may be
#' present on the server.
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#'
#' # connect and list files
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#'
#' # inspect parsed metadata
#' str(mc_files)
#'
#' # inspect ignored files
#' attr(mc_files, "ignored_files")
#'
#' }
get_measurementchains_files <- function(
    sftp_connection = create_sftp_connection(),
    debug = FALSE
)
{
  file_info <- sftp_connection %>%
    list_sftp_files(debug = debug) %>%
    kwb.utils::renameColumns(list(name = "sftp_path"))
  
  folder_file <- file_info %>%
    kwb.utils::selectColumns("sftp_path") %>%
    split_into_folder_and_file()
  
  file_stem <- folder_file$file %>%
    kwb.utils::removeExtension()
  
  validation <- validate_measurementchain_files(file_stem, debug = debug)
  
  ok <- validation$ok
  
  # Filter synchron
  file_info   <- file_info[ok, , drop = FALSE]
  folder_file <- folder_file[ok, , drop = FALSE]
  file_stem   <- file_stem[ok]
  
  galery_well <- folder_file %>%
    kwb.utils::selectColumns("folder") %>%
    split_into_galery_and_well()
  
  sensor_date_time <- file_stem %>%
    split_into_sensor_and_datetime()
  
  result <- file_info %>%
    cbind(galery_well, sensor_date_time) %>%
    kwb.utils::resetRowNames()
  
  # attach ignored files log
  attr(result, "ignored_files") <- validation$ignored
  
  result
}

#' List files on SFTP server (FAST, assumes flat Kxx folders)
#'
#' Root contains directories K09/K10/K13, each contains data files (CSV etc).
#' No recursion and no per-entry probing (massively faster).
#'
#' @keywords internal
#' @noRd
#' @importFrom curl curl_fetch_memory new_handle
#' @importFrom kwb.utils stopFormatted
list_sftp_files <- function(
    sftp_connection = create_sftp_connection(),
    debug = FALSE
)
{
  remote_dir <- Sys.getenv("MESSKETTEN_REMOTE_DIR", unset = ".")
  remote_dir <- trimws(remote_dir)
  
  # Treat "." as root
  if (!nzchar(remote_dir) || remote_dir %in% c(".", "./")) remote_dir <- ""
  remote_dir <- sub("^\\./+", "", remote_dir)
  remote_dir <- sub("^/+", "", remote_dir)
  
  url_encode <- function(x) {
    bytes <- charToRaw(x)
    out <- vapply(as.integer(bytes), function(b) {
      ch <- rawToChar(as.raw(b))
      if (grepl("^[A-Za-z0-9\\-\\._~]$", ch)) ch else sprintf("%%%02X", b)
    }, character(1))
    paste0(out, collapse = "")
  }
  encode_path <- function(p) {
    p <- gsub("\\\\", "/", p)
    seg <- strsplit(p, "/", fixed = TRUE)[[1]]
    paste0(vapply(seg, url_encode, ""), collapse = "/")
  }
  normalize_name <- function(x) {
    x <- trimws(x)
    x <- sub("^\\./+", "", x)
    x <- sub("^/+", "", x)
    x
  }
  ensure_slash <- function(u) if (endsWith(u, "/")) u else paste0(u, "/")
  
  port <- as.integer(sftp_connection$port %||% 22L)
  
  base_url <- sprintf(
    "sftp://%s:%s@%s:%d/%s",
    url_encode(sftp_connection$username),
    url_encode(sftp_connection$password),
    sftp_connection$server,
    port,
    encode_path(remote_dir)
  )
  base_url <- ensure_slash(sub("/+$", "/", base_url))
  
  curl_list_dir <- function(dir_url) {
    dir_url <- ensure_slash(dir_url)
    if (isTRUE(debug)) message("LIST: ", dir_url)
    h <- curl::new_handle()
    res <- curl::curl_fetch_memory(dir_url, handle = h)
    rawToChar(res$content)
  }
  
  # Parse names from listing (ls -l, plain, MLSD-ish)
  parse_names <- function(txt) {
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    lines <- lines[!grepl("^total\\s+", lines)]
    if (!length(lines)) return(character(0))
    
    if (any(grepl("(^|;)type=(file|dir);", lines, ignore.case = TRUE))) {
      nm <- sub("^.*;\\s*", "", lines)
      nm <- sub("^.*\\s+", "", nm)
    } else {
      nm <- sub("^.*\\s+", "", lines)
    }
    
    nm <- normalize_name(nm)
    nm <- nm[!nm %in% c(".", "..")]
    unique(nm[nzchar(nm)])
  }
  
  # 1) Root dirs: keep only Kxx
  root_txt <- try(curl_list_dir(base_url), silent = TRUE)
  if (inherits(root_txt, "try-error")) {
    kwb.utils::stopFormatted("Failed listing SFTP root dir: %s\n%s", base_url, as.character(root_txt))
  }
  root_entries <- parse_names(root_txt)
  k_dirs <- root_entries[grepl("^K\\d{2}$", root_entries)]
  
  if (!length(k_dirs)) {
    kwb.utils::stopFormatted(
      "No Kxx directories found in SFTP root. Got: %s",
      paste(utils::head(root_entries, 30), collapse = ", ")
    )
  }
  
  # 2) List each Kxx folder ONCE (no probing)
  all_files <- unlist(lapply(k_dirs, function(d) {
    txt <- try(curl_list_dir(paste0(base_url, encode_path(d), "/")), silent = TRUE)
    if (inherits(txt, "try-error")) {
      kwb.utils::stopFormatted("Failed listing SFTP dir: %s%s/\n%s", base_url, d, as.character(txt))
    }
    files <- parse_names(txt)
    # Drop nested dirs if any accidentally appear (cheap heuristic):
    files <- files[!grepl("^K\\d{2}$", files)]
    file.path(d, files)
  }), use.names = FALSE)
  
  all_files <- gsub("\\\\", "/", all_files)
  all_files <- all_files[nzchar(all_files)]
  
  data.frame(
    name = unique(all_files),
    type = "file",
    stringsAsFactors = FALSE
  )
}

# split_into_folder_and_file ---------------------------------------------------
split_into_folder_and_file <- function(x)
{
  data.frame(
    folder = dirname(x), 
    file = basename(x)
  )
}

# split_into_galery_and_well ---------------------------------------------------
split_into_galery_and_well <- function(x)
{
  data.frame(
    galerie = substr(x, 1L, 1L), 
    brunnen_nummer = as.integer(substr(x, 2L, nchar(x)))
  )
}

#' Validate measurement chain filenames
#'
#' @keywords internal
#' @noRd
validate_measurementchain_files <- function(file_stem, debug = FALSE)
{
  pattern <- "^(LF_)?(\\d{6})(\\d)[-_](\\d{4}-\\d{2}-\\d{2}-\\d{4})$"
  
  ok <- grepl(pattern, file_stem)
  
  ignored <- tibble::tibble(
    file = file_stem[!ok],
    reason = "filename does not match measurement chain pattern"
  )
  
  if (nrow(ignored) > 0 && isTRUE(debug)) {
    message(sprintf(
      "Ignoring %d files that do not match measurement chain naming scheme.",
      nrow(ignored)
    ))
    message("Examples: ", paste(utils::head(ignored$file, 20), collapse = ", "))
  }
  
  list(
    ok = ok,
    ignored = ignored
  )
}

# split_into_sensor_and_datetime -----------------------------------------------
split_into_sensor_and_datetime <- function(x)
{
  pattern <- "^(LF_)?(\\d{6})(\\d)[-_](\\d{4}-\\d{2}-\\d{2}-\\d{4})$"
  
  extracted <- kwb.utils::extractSubstring(
    pattern, x,
    c(prefix = 1L,
      sensor_id = 2L,
      sensor_endnummer = 3L,
      datum_uhrzeit = 4L)
  )
  
  extracted %>%
    dplyr::mutate(
      sensor_id = as.integer(.data$sensor_id),
      sensor_endnummer = as.integer(.data$sensor_endnummer),
      datum_uhrzeit = as_gmt_plus_one(
        .data$datum_uhrzeit,
        format = "%Y-%m-%d-%H%M"
      )
    )
}

#' @keywords internal
#' @noRd
url_encode <- function(x)
{
  # keep it dependency-light; encode everything except unreserved
  # RFC 3986 unreserved: A-Z a-z 0-9 -._~
  # We also keep "/" for paths.
  bytes <- charToRaw(x)
  out <- vapply(as.integer(bytes), function(b) {
    ch <- rawToChar(as.raw(b))
    if (grepl("^[A-Za-z0-9\\-\\._~]$", ch)) ch else sprintf("%%%02X", b)
  }, character(1))
  paste0(out, collapse = "")
}

#' @keywords internal
#' @noRd
build_sftp_url <- function(server, username, password, remote_path = "", port = 22L)
{
  user_enc <- url_encode(username)
  pass_enc <- url_encode(password)
  
  # remote_path may contain slashes; we encode each segment but keep "/" intact
  remote_path <- gsub("\\\\", "/", remote_path)
  remote_path <- paste0(vapply(strsplit(remote_path, "/", fixed = TRUE)[[1]], url_encode, ""), collapse = "/")
  remote_path <- sub("^/+", "", remote_path)
  
  sprintf("sftp://%s:%s@%s:%d/%s", user_enc, pass_enc, server, port, remote_path)
}

#' Measurement Chains: download data (SFTP via curl)
#'
#' @param sftp_paths character vector with relative paths to files (under MESSKETTEN_REMOTE_DIR)
#' @param target_directory target directory
#' @param run_parallel default: TRUE (will create separate curl handles; ok)
#' @param debug show debug messages (default: FALSE)
#' @return character vector of local file paths
#' @export
#'
#' @importFrom fs dir_create
#' @importFrom kwb.utils catAndRun isTryError stopFormatted createDirectory
#' @importFrom parallel detectCores makeCluster stopCluster parLapply
#' @importFrom curl curl_download
download_measurementchains_data <- function(
    sftp_paths,
    target_directory = temp_dir(),
    run_parallel = TRUE,
    debug = FALSE
)
{
  con <- get_environment_variables(
    server   = "MESSKETTEN_SERVER",
    username = "MESSKETTEN_USER",
    password = "MESSKETTEN_PASSWORD",
    check. = TRUE
  )
  
  port <- Sys.getenv("MESSKETTEN_PORT", unset = "22")
  port <- suppressWarnings(as.integer(port))
  if (is.na(port) || port <= 0L) port <- 22L
  
  remote_dir <- Sys.getenv("MESSKETTEN_REMOTE_DIR", unset = ".")
  remote_dir <- if (nzchar(remote_dir)) remote_dir else "."
  
  to_target_path <- function(x) file.path(target_directory, x)
  kwb.utils::createDirectory(target_directory, dbg = debug)
  
  paths_to_download <- exclude_existing_paths(sftp_paths, target_directory)
  if (!length(paths_to_download)) return(to_target_path(sftp_paths))
  
  download_one <- function(rel_path) {
    local_path <- to_target_path(rel_path)
    fs::dir_create(dirname(local_path), recurse = TRUE)
    
    remote_path <- file.path(remote_dir, rel_path)
    remote_path <- gsub("\\\\", "/", remote_path)
    
    url <- build_sftp_url(
      server = con$server,
      username = con$username,
      password = con$password,
      remote_path = remote_path,
      port = port
    )
    
    if (isTRUE(debug)) message("curl_download: ", url, " -> ", local_path)
    
    try(curl::curl_download(url, destfile = local_path, quiet = !isTRUE(debug)))
  }
  
  ncores <- max(1L, parallel::detectCores() - 1L)
  do_run_parallel <- isTRUE(run_parallel) && ncores > 1L
  
  result_list <- kwb.utils::catAndRun(
    sprintf(
      "Download %d measurement chains files via SFTP (curl) to %s (using %d core%s)",
      length(paths_to_download), target_directory, ncores, ifelse(ncores > 1L, "s", "")
    ),
    dbg = debug,
    expr = if (do_run_parallel) {
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::parLapply(cl, paths_to_download, download_one)
    } else {
      lapply(paths_to_download, download_one)
    }
  )
  
  failed <- vapply(result_list, kwb.utils::isTryError, logical(1))
  if (all(failed)) {
    kwb.utils::stopFormatted(
      "Download for all %d files failed via curl SFTP.\nFirst error:\n%s",
      length(paths_to_download),
      as.character(result_list[[which(failed)[1]]])
    )
  }
  if (any(failed)) {
    message("Failed downloading the following path(s):")
    message(paste0(paths_to_download[failed], collapse = "\n"))
  }
  
  to_target_path(sftp_paths[!failed])
}

# exclude_existing_paths -------------------------------------------------------
exclude_existing_paths <- function(paths, target)
{
  #target <- kwb.geosalz:::temp_dir("R_kwb.geosalz/download")
  existing <- dir(target, recursive = TRUE)
  
  common <- intersect(paths, existing)
  n_common <- length(common)
  
  if (n_common) {
    message(sprintf("Exclude %d paths that already exist locally.", n_common))
    paths <- setdiff(paths, common)
  }
  
  paths
}

#' Measurement Chain: read csv data from a single file (robust)
#'
#' @keywords internal
#' @noRd
#' @importFrom readr read_csv cols col_datetime col_integer col_double locale
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
read_measurementchain_data <- function(path, debug = FALSE)
{
  # Helper: attach error info in a consistent format
  mk_err <- function(reason, details = NULL, cols = NULL) {
    structure(
      NULL,
      mc_error = list(
        path = path,
        reason = reason,
        details = details,
        cols = cols
      )
    )
  }
  
  # Try read
  x <- try(
    readr::read_csv(
      path,
      locale = readr::locale(tz = "Etc/GMT-1"),
      col_types = readr::cols(
        Geraet = readr::col_integer(),
        DatumUhrzeit = readr::col_datetime(),
        Leitfaehigkeit = readr::col_double(),
        Temperatur = readr::col_double()
      )
    ),
    silent = TRUE
  )
  
  if (inherits(x, "try-error")) {
    if (isTRUE(debug)) message("READ FAIL: ", path, " | ", as.character(x))
    return(mk_err("read_csv_failed", details = as.character(x)))
  }
  
  # Validate expected columns before rename/pivot
  need <- c("Geraet", "DatumUhrzeit", "Leitfaehigkeit", "Temperatur")
  have <- names(x)
  missing <- setdiff(need, have)
  
  if (length(missing) > 0) {
    if (isTRUE(debug)) {
      message("SCHEMA FAIL: ", path)
      message("  missing: ", paste(missing, collapse = ", "))
      message("  have: ", paste(utils::head(have, 50), collapse = ", "))
    }
    return(mk_err("missing_required_columns", details = paste(missing, collapse = ","), cols = have))
  }
  
  # Transform (no pipes)
  x <- dplyr::rename(
    x,
    sensor_id = "Geraet",
    datum_uhrzeit = "DatumUhrzeit"
  )
  
  out <- try(
    tidyr::pivot_longer(
      x,
      names_to = "parameter",
      values_to = "messwert",
      cols = tidyselect::all_of(c("Leitfaehigkeit", "Temperatur"))
    ),
    silent = TRUE
  )
  
  if (inherits(out, "try-error")) {
    if (isTRUE(debug)) message("PIVOT FAIL: ", path, " | ", as.character(out))
    return(mk_err("pivot_failed", details = as.character(out), cols = names(x)))
  }
  
  out
}

#' Measurement Chains: read csv data from multiple files
#'
#' @param csv_files vector of paths as retrieved by
#'   \code{\link{download_measurementchains_data}}
#' @param datetime_installation datetime of first logger installation in well K10. 
#' Used to filter out older measurement data! Default: 
#' kwb.geosalz:::as_gmt_plus_one("2022-09-27 11:00:00")
#' @param run_parallel default: TRUE
#' @param debug show debug messages (default: FALSE)
#' @return data frame with imported data from csv files
#' @export
#' @importFrom kwb.file remove_common_root
#' @importFrom kwb.utils catAndRun isNullOrEmpty
#' @importFrom readr read_csv col_datetime
#' @importFrom dplyr arrange mutate bind_rows
#' @examples
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' target_directory <- tempdir()
#' csv_files <- kwb.geosalz::download_measurementchains_data(
#'   sftp_paths = mc_files$sftp_path,
#'   target_directory
#' )
#' mc_data <- kwb.geosalz::read_measurementchains_data(csv_files)
#' }
read_measurementchains_data <- function(
    csv_files,
    datetime_installation = as_gmt_plus_one("2022-09-27 11:00:00"),
    run_parallel = TRUE,
    debug = FALSE
)
{
  files_exist <- fs::file_exists(csv_files)
  if (!all(files_exist)) {
    kwb.utils::stopFormatted(
      paste0(
        "The following %d (out of %d) local csv files do not exist:\n\n%s\n\n",
        "Please run kwb.geosalz::download_measurementchains_data() again!"
      ),
      sum(!files_exist),
      length(csv_files),
      paste0(csv_files[!files_exist], collapse = "\n")
    )
  }
  
  ncores <- max(1L, parallel::detectCores() - 1L)
  do_run_parallel <- isTRUE(run_parallel) && ncores > 1L
  
  if (do_run_parallel) {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
    # Ensure workers have the current function object
    parallel::clusterExport(
      cl,
      varlist = c("read_measurementchain_data"),
      envir = environment()
    )
  }
  
  worker <- function(p) read_measurementchain_data(p, debug = debug)
  
  result_list <- kwb.utils::catAndRun(
    sprintf(
      "Importing %d measurement chains files (using %d CPU core%s)",
      length(csv_files),
      if (do_run_parallel) ncores else 1L,
      ifelse((if (do_run_parallel) ncores else 1L) > 1L, "s", "")
    ),
    dbg = debug,
    expr = if (do_run_parallel) {
      parallel::parLapply(cl, csv_files, worker)
    } else {
      lapply(csv_files, worker)
    }
  )
  
  names(result_list) <- kwb.file::remove_common_root(csv_files, dbg = FALSE)
  
  # Collect errors (NULL entries with mc_error attribute)
  errs <- lapply(result_list, function(x) attr(x, "mc_error", exact = TRUE))
  bad_idx <- vapply(errs, function(e) !is.null(e), logical(1))
  bad_files <- tibble::tibble(
    file = names(result_list)[bad_idx],
    path = vapply(errs[bad_idx], `[[`, "", "path"),
    reason = vapply(errs[bad_idx], `[[`, "", "reason"),
    details = vapply(errs[bad_idx], function(e) if (is.null(e$details)) NA_character_ else e$details, ""),
    cols = vapply(errs[bad_idx], function(e) {
      if (is.null(e$cols)) NA_character_ else paste(e$cols, collapse = ",")
    }, "")
  )
  
  # Keep only successful imports
  ok_list <- result_list[!bad_idx]
  
  if (isTRUE(debug) && nrow(bad_files) > 0) {
    message(sprintf("Skipped %d files with schema/read errors.", nrow(bad_files)))
    print(utils::head(bad_files, 20))
  }
  
  result <- dplyr::bind_rows(ok_list, .id = "file")
  result <- order_measurement_chain_data(result)
  
  attr(result, "bad_files") <- bad_files
  
  if (kwb.utils::isNullOrEmpty(datetime_installation)) {
    return(result)
  }
  
  remove_measurements_before(
    result,
    datetime = datetime_installation,
    reason = "installation in K10"
  )
}
# order_measurement_chain_data -------------------------------------------------

#' Order Measurement Chain Data
#' 
#' @param data data frame as retrieved by
#'   \code{\link{read_measurementchains_data}}
#' @return \code{data}, ordered by "parameter", "sensor_id", "datum_uhrzeit"
#' @importFrom kwb.utils orderBy
#' @export
order_measurement_chain_data <- function(data)
{
  kwb.utils::orderBy(data, c("parameter", "sensor_id", "datum_uhrzeit"))
}

# remove_measurements_before ---------------------------------------------------
remove_measurements_before <- function(
    data, 
    datetime, 
    reason = "Why?", 
    debug = TRUE
)
{
  kwb.utils::catAndRun(
    sprintf(
      "Filtering out 'lab' measurements before '%s' (%s)", 
      datetime, 
      reason
    ),
    dbg = debug,
    expr = dplyr::filter(data, .data$datum_uhrzeit >= datetime),
  )
}
