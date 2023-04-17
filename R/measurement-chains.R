#' Measurement Chains: Get Metadata
#'
#' @param file path to measurement chains metadata file (default:
#' system.file("extdata/metadata_messketten.csv", package = "kwb.geosalz"))
#' @return tibble with measurement chains metadata
#' @export
#' @importFrom readr cols col_character col_integer col_double read_csv
#' @examples
#' mc_metadata <- kwb.geosalz::get_measurementchains_metadata()
#' str(mc_metadata)
#' mc_metadata
get_measurementchains_metadata <- function(
    file = system.file(
      "extdata/metadata_messketten.csv", 
      package = "kwb.geosalz"
    )
)
{
  readr::read_csv(
    file = file,
    col_types = readr::cols(
      "galerie" = readr::col_character(),
      "brunnen_nummer" = readr::col_integer(),
      "dn" = readr::col_integer(),
      "einbau_pumpe" = readr::col_character(),
      "einbau_messkette" = readr::col_character(),
      "filteroberkante_muGOK" = readr::col_double(),
      "filterunterkante_muGOK" = readr::col_double(),
      "sensor_id" = readr::col_integer(),
      "sensor_endnummer" = readr::col_integer(),
      "einbau_sensor_muGOK" = readr::col_double()
    )
  )
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
  con_vars <- c(
    server = "MESSKETTEN_SERVER", 
    username = "MESSKETTEN_USER", 
    password = "MESSKETTEN_PASSWORD"
  )
  
  con <- do.call(get_environment_variables, as.list(con_vars))
  
  not_defined <- sapply(con, stringr::str_length) == 0L
  
  if (any(not_defined)) {
    kwb.utils::stopFormatted(
      "The following required environment variables are undefined/empty:\n%s",
      paste0(con_vars[not_defined], collapse = ", ")
    )
  }
  
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
  files <- sftp::sftp_list(
    sftp_connection, 
    recurse = TRUE, 
    verbose = debug
  )
  
  files %>%
    dplyr::filter(.data$type != "dir") %>%
    tidyr::separate(
      .data$name,
      into = c("galerywellid", "sensorid_datetime"),
      sep = "/",
      remove = FALSE
    ) %>%
    dplyr::rename(sftp_path = .data$name) %>%
    dplyr::mutate(
      galerie = stringr::str_extract(.data$galerywellid, "^[A-Z]"),
      brunnen_nummer = stringr::str_extract(
        .data$galerywellid, 
        "[0-9]{1,3}$"
      ) %>%
        as.integer()
    ) %>%
    dplyr::mutate(
      sensorid_datetime = stringr::str_replace(
        .data$sensorid_datetime,
        pattern = "-202",
        "_202"
      )
    ) %>%
    tidyr::separate(
      .data$sensorid_datetime,
      into = c("sensor_id", "datum_uhrzeit"),
      sep = "_"
    ) %>%
    dplyr::mutate(
      sensor_endnummer = stringr::str_extract(
        .data$sensor_id, 
        "[0-9]$"
      ) %>%
        as.integer(),
      sensor_id = as.integer(.data$sensor_id),
      datum_uhrzeit = stringr::str_remove(.data$datum_uhrzeit, "\\.csv$")
    ) %>%
    dplyr::mutate(datum_uhrzeit = as.POSIXct(
      .data$datum_uhrzeit,
      format = "%Y-%m-%d-%H%M",
      #data is always CET without switching
      #https://stackoverflow.com/a/38333522
      tz = "Etc/GMT-1"
    )) %>%
    dplyr::select(-.data$galerywellid) %>%
    tibble::as_tibble()
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
    target_directory = tempdir(),
    sftp_connection = create_sftp_connection(),
    run_parallel = TRUE,
    debug = FALSE
)
{
  fs::dir_create(target_directory)
  
  debug_message <- function(ncores) {
    sprintf(
      "Download %d measurement chains files (using %d CPU core%s)",
      length(sftp_paths),
      ncores, 
      ifelse(ncores > 1L, "s", "")
    )
  }
  
  if (run_parallel) {
    
    ncores <- parallel::detectCores()
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
  } else {
    
    ncores <- 1L
  }

  try_to_download <- function(file, verbose) {
    try(sftp::sftp_download(
      file = file,
      sftp_connection = sftp_connection,
      tofolder = target_directory,
      verbose = verbose
    ))
  }

  dl_list <- kwb.utils::catAndRun(
    debug_message(ncores),
    dbg = debug,
    expr = if (run_parallel) {
      parallel::parLapply(cl, sftp_paths, function(file) {
        try_to_download(file, verbose = FALSE)
      })
    } else {
      lapply(sftp_paths, function(file) {
        try_to_download(file, verbose = debug)
      })
    }
  )

  failed <- sapply(dl_list, kwb.utils::isTryError)
  
  if (any(failed)) {
    message("Failed downloading data from the following FTP path(s):")
    message(paste0(sftp_paths[failed], collapse = "\n"))
  }
  
  if (all(failed)) {
    kwb.utils::stopFormatted(
      "Download for all %d measurement chains files failed!",
      length(sftp_paths)
    )
  }
  
  csv_paths <- sapply(sftp_paths[!failed], function(sftp_path) {
    fs::path_join(parts = c(target_directory, sftp_path))
  })
  
  tibble::tibble(
    file_id = seq_along(csv_paths),
    sftp_path = names(csv_paths),
    local_path = as.character(csv_paths)
  )
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
  readr::read_csv(
    file = path,
    locale = readr::locale(
      #data is always CET without switching
      #https://stackoverflow.com/a/38333522
      tz = "Etc/GMT-1"
    ),
    col_types = readr::cols(
      "Geraet" = readr::col_integer(),
      "DatumUhrzeit" = readr::col_datetime(),
      "Leitfaehigkeit" = readr::col_double(),
      "Temperatur" = readr::col_double()
    )
  ) %>%
    dplyr::rename(
      sensor_id = .data$Geraet,
      datum_uhrzeit = .data$DatumUhrzeit
    ) %>%
    tidyr::pivot_longer(
      names_to = "parameter",
      values_to = "messwert",
      cols = tidyselect::all_of(c("Leitfaehigkeit", "Temperatur"))
    )
}

#' Measurement Chains: read csv data from multiple files
#'
#' @param csv_files tibble as retrieved by \code{\link{download_measurementchains_data}}
#' @param datetime_installation datetime of first logger installation in well K10. 
#' Used to filter out older measurement data! (default: as.POSIXct("2022-09-27 11:00:00", 
#' tz = "Etc/GMT-1")
#' @param debug show debug messages (default: FALSE)
#' @return data frame with imported data from csv files
#' @export
#' @importFrom kwb.utils catAndRun isNullOrEmpty
#' @importFrom readr read_csv col_datetime
#' @importFrom dplyr arrange mutate bind_rows
#' @examples
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' target_directory <- tempdir()
#' csv_files <- kwb.geosalz::download_measurementchains_data(
#' sftp_paths = mc_files$sftp_path,
#' target_directory)
#' mc_data <- kwb.geosalz::read_measurementchains_data(csv_files)
#'
#' }
read_measurementchains_data <- function(
    csv_files,
    datetime_installation = as.POSIXct("2022-09-27 11:00:00", tz = "Etc/GMT-1"),
    run_parallel = TRUE,
    debug = FALSE
) 
{
  files_exist <- fs::file_exists(csv_files$local_path)
  
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
  
  debug_message <- function(ncores) {
    sprintf(
      "Importing %d measurement chains files (using %d CPU core%s)",
      nrow(csv_files),
      ncores,
      ifelse(ncores > 1L, "s", "")
    )
  }
  
  if (run_parallel) {
    
    ncores <- parallel::detectCores()
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
  } else {
    
    ncores <- 1L
  }
  
  files <- stats::setNames(
    kwb.utils::selectColumns(csv_files, "local_path"),
    kwb.utils::selectColumns(csv_files, "file_id")
  )
  
  data_list <- kwb.utils::catAndRun(
    debug_message(ncores),
    dbg = debug,
    expr = if (run_parallel) {
      parallel::parLapply(cl, files, read_measurementchain_data)
    } else {
      lapply(files, read_measurementchain_data)
    }
  )
  
  result <- data_list %>%
    dplyr::bind_rows(.id = "file_id") %>%
    dplyr::mutate(file_id = as.integer(.data$file_id)) %>%
    order_measurement_chain_data()
  
  if (kwb.utils::isNullOrEmpty(datetime_installation)) {
    return(result)  
  }
  
  kwb.utils::catAndRun(
    sprintf(
      "Filtering out 'lab' measurements before '%s' (installation in K10)", 
      datetime_installation
    ),
    dbg = debug,
    expr = dplyr::filter(result, .data$datum_uhrzeit >= datetime_installation),
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
