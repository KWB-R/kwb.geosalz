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

#' Measurement Chains: Create an SFTP Connection
#'
#' @return sftp connection
#' @export
#' @importFrom kwb.utils stopFormatted
#' @importFrom sftp sftp_connect
#' @importFrom stringr str_length
create_sftp_connection <- function()
{
  con <- get_environment_variables(
    server = "MESSKETTEN_SERVER", 
    username = "MESSKETTEN_USER", 
    password = "MESSKETTEN_PASSWORD",
    check. = TRUE
  )

  do.call(sftp::sftp_connect, con)
}

#' Measurement Chains: Get Tidied Files Metadata
#'
#' @param sftp_connection an SFTP connnection as retrieved by
#' \code{\link{create_sftp_connection}}
#' @param debug show debug messages (default: FALSE)
#'
#' @return tibble with information on available files and tidied meta-information
#' based on file naming
#' @export
#'
#' @importFrom sftp sftp_list
#' @importFrom dplyr filter rename mutate select
#' @importFrom tidyr separate
#' @importFrom tibble as_tibble
#' @importFrom stringr str_extract str_remove
#' @examples
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' str(mc_files)
#' }
get_measurementchains_files <- function(
    sftp_connection = create_sftp_connection(),
    debug = FALSE
)
{
  file_info <- sftp_connection %>%
    list_sftp_files() %>%
    kwb.utils::renameColumns(list(name = "sftp_path"))

  folder_file <- file_info %>%
    kwb.utils::selectColumns("sftp_path") %>%
    split_into_folder_and_file()
  
  galery_well <- folder_file %>%
    kwb.utils::selectColumns("folder") %>%
    split_into_galery_and_well()

  sensor_date_time <- folder_file %>%
    kwb.utils::selectColumns("file") %>%
    kwb.utils::removeExtension() %>%
    split_into_sensor_and_datetime()

  file_info %>%
    cbind(galery_well, sensor_date_time) %>%
    kwb.utils::resetRowNames()
}

# list_sftp_files --------------------------------------------------------------
list_sftp_files <- function(
    sftp_connection = create_sftp_connection(), 
    debug = FALSE
)
{
  sftp_connection %>% 
    sftp::sftp_list(recurse = TRUE, verbose = debug) %>%
    dplyr::filter(.data[["type"]] != "dir")
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

# split_into_sensor_and_datetime -----------------------------------------------
split_into_sensor_and_datetime <- function(x)
{
  pattern <- "^(LF_)?(\\d{6}(\\d))[-_](\\d{4}-\\d{2}-\\d{2}-\\d{4})$"
  
  stopifnot(all(grepl(pattern, x)))
  
  kwb.utils::extractSubstring(pattern, x, c(
    prefix = 1L, 
    sensor_id = 2L, 
    sensor_endnummer = 3L,
    datum_uhrzeit = 4L
  )) %>%
    dplyr::mutate(
      sensor_id = as.integer(.data[["sensor_id"]]),
      sensor_endnummer = as.integer(.data[["sensor_endnummer"]]),
      datum_uhrzeit = as_gmt_plus_one(
        .data[["datum_uhrzeit"]], 
        format = "%Y-%m-%d-%H%M",
      )
    )
}

#' Measurement Chains: download data
#'
#' @param sftp_paths character vector with paths to files to be downloaded. As
#' retrieved by \code{\link{get_measurementchains_files}} column "sftp_path"
#' @param target_directory target directory
#' @param sftp_connection an SFTP connnection as retrieved by
#' \code{\link{create_sftp_connection}}
#' @param run_parallel default: TRUE
#' @param debug show debug messages (default: FALSE)
#'
#' @return tibble with columns file_id, sftp_path and local_path of csv files
#' @export
#' @importFrom fs dir_create
#' @importFrom kwb.utils catAndRun isTryError stopFormatted
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom sftp sftp_download
#' @examples
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' target_directory <- tempdir()
#' local_paths <- kwb.geosalz::download_measurementchains_data(
#' sftp_paths = mc_files$sftp_path,
#' target_directory)
#' }
download_measurementchains_data <- function(
    sftp_paths,
    target_directory = temp_dir(),
    sftp_connection = create_sftp_connection(),
    run_parallel = TRUE,
    debug = FALSE
)
{
  # Helper function to create the full paths in the target directory
  to_target_path <- function(x) file.path(target_directory, x)

  # Helper function to download one file from the SFTP server
  try_to_download <- function(path, verbose) {
    try(sftp::sftp_download(
      file = path,
      sftp_connection = sftp_connection,
      tofolder = target_directory,
      verbose = verbose
    ))
  }
  
  # Create the target directory if it does not exist
  kwb.utils::createDirectory(target_directory, dbg = debug)

  # Exclude paths that already exist in the target directory
  paths_to_download <- exclude_existing_paths(sftp_paths, target_directory)

  # Return early if there is nothing to do  
  if (length(paths_to_download) == 0L) {
    return(to_target_path(sftp_paths))
  }

  # Number of cores to use  
  ncores <- parallel::detectCores() - 1L
  
  # Can we do parallel processing?
  do_run_parallel <- run_parallel && ncores > 1L
  
  # Prepare parallel processing if required
  if (do_run_parallel) {
    
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
  } else {
    
    ncores <- 1L
  }

  # Call the download function in a (parallel or sequential) loop
  result_list <- kwb.utils::catAndRun(
    sprintf(
      "Download %d measurement chains files (using %d CPU core%s) to %s",
      length(paths_to_download),
      ncores, 
      ifelse(ncores > 1L, "s", ""),
      target_directory
    ),
    dbg = debug,
    expr = if (do_run_parallel) {
      parallel::parLapply(
        cl = cl, 
        X = paths_to_download, 
        fun = try_to_download, 
        verbose = FALSE
      )
    } else {
      lapply(
        X = paths_to_download, 
        FUN = try_to_download, 
        verbose = debug
      )
    }
  )

  # For which files did the download fail?  
  failed <- sapply(result_list, kwb.utils::isTryError)
  
  if (all(failed)) {
    kwb.utils::stopFormatted(
      "Download for all %d measurement chains files failed!",
      length(sftp_paths)
    )
  }
  
  if (any(failed)) {
    message("Failed downloading data from the following FTP path(s):")
    message(paste0(sftp_paths[failed], collapse = "\n"))
  }

  # Return the local paths to the downloaded files  
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

#' Measurement Chain: read csv data from a single files
#'
#' @param path path to local csv file as retrieved by
#' \code{\link{download_measurementchains_data}} in column "local_path"
#' @return data frame with imported data from csv file
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom readr read_csv cols col_datetime col_integer col_character
#' col_double
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom tidyselect all_of
#' @examples
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' target_directory <- tempdir()
#' csv_files <- kwb.geosalz::download_measurementchains_data(
#' sftp_paths = mc_files$sftp_path,
#' target_directory)
#' mc_data <- kwb.geosalz::read_measurementchain_data(csv_files[1])
#'
#' }
read_measurementchain_data <- function(path)
{
  path %>%
    readr::read_csv(
      # data is always CET without switching
      # https://stackoverflow.com/a/38333522
      locale = readr::locale(tz = "Etc/GMT-1"),
      col_types = readr::cols(
        Geraet = readr::col_integer(),
        DatumUhrzeit = readr::col_datetime(),
        Leitfaehigkeit = readr::col_double(),
        Temperatur = readr::col_double()
      )
    ) %>%
    dplyr::rename(
      sensor_id = "Geraet",
      datum_uhrzeit = "DatumUhrzeit"
    ) %>%
    tidyr::pivot_longer(
      names_to = "parameter",
      values_to = "messwert",
      cols = tidyselect::all_of(c("Leitfaehigkeit", "Temperatur"))
    )
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
        "The following %d (out of %d) local csv files do not ",
        "exist:\n\n%s\n\nPlease run kwb.geosalz::download_measurementchains_data() ",
        "again!"
      ),
      sum(!files_exist),
      nrow(csv_files),
      paste0(csv_files$local_path[!files_exist], collapse = "\n")
    )
  }
  
  ncores <- parallel::detectCores() - 1L
  
  # Can we do parallel processing?
  do_run_parallel <- run_parallel && ncores > 1L
  
  # Prepare parallel processing if required
  if (do_run_parallel) {
    
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
  } else {
    
    ncores <- 1L
  }
  
  result_list <- kwb.utils::catAndRun(
    sprintf(
      "Importing %d measurement chains files (using %d CPU core%s)",
      length(csv_files),
      ncores,
      ifelse(ncores > 1L, "s", "")
    ),
    dbg = debug,
    expr = if (do_run_parallel) {
      parallel::parLapply(cl, csv_files, read_measurementchain_data)
    } else {
      lapply(csv_files, read_measurementchain_data)
    }
  )

  names(result_list) <- kwb.file::remove_common_root(csv_files, dbg = FALSE)
  
  result <- result_list %>%
    dplyr::bind_rows(.id = "file") %>%
    order_measurement_chain_data()
  
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
