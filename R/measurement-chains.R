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
  con_vars <- sprintf("MESSKETTEN_%s", c("SERVER", "USER", "PASSWORD"))
  
  con <- list(
    server = Sys.getenv(con_vars[1L]),
    username = Sys.getenv(con_vars[2L]),
    password = Sys.getenv(con_vars[3L])
  )
  
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
  files <- sftp::sftp_list(sftp_connection, recurse = TRUE, verbose = debug) 
  
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
      brunnen_nummer = stringr::str_extract(.data$galerywellid, "[0-9]{1,3}$") %>% 
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
      sensor_endnummer = stringr::str_extract(.data$sensor_id, "[0-9]$") %>% 
        as.integer(),  
      sensor_id = as.integer(.data$sensor_id), 
      datum_uhrzeit = stringr::str_remove(.data$datum_uhrzeit, "\\.csv$")
    ) %>%  
    dplyr::mutate(
      datum_uhrzeit = as.POSIXct(
        .data$datum_uhrzeit, 
        format = "%Y-%m-%d-%H%M", 
        #data is always CET without switching
        #https://stackoverflow.com/a/38333522                                           
        tz = "Etc/GMT-1"
      )
    ) %>% 
    dplyr::select(- .data$galerywellid) %>% 
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
#' @return paths to downloaded files
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
  
  msg <- sprintf("Importing %d measurement chains files", length(sftp_paths))  
  
  if (run_parallel) {
    
    ncores <- parallel::detectCores()
    
    cl <- parallel::makeCluster(ncores)
    
    dl_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = parallel::parLapply(
        cl, sftp_paths, function(sftp_path) {
          try(sftp::sftp_download(
            file = sftp_path,
            sftp_connection = sftp_connection,
            tofolder = target_directory, 
            verbose = FALSE
          ))
        }),
      dbg = debug
    )
    
    parallel::stopCluster(cl)
    
  } else {
    
    dl_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = lapply(sftp_paths, function(sftp_path) {
        try(sftp::sftp_download(
          file = sftp_path,
          sftp_connection = sftp_connection,
          tofolder = target_directory, 
          verbose = debug
        ))
      }),
      dbg = debug
    )
  }
  
  failed <- sapply(dl_list, kwb.utils::isTryError)
  
  if (any(failed)) {
    message("Failed downloading data from the following FTP path(s):")
    message(paste0(sftp_paths[failed], collapse = "\n"))
  }  
  
  if (all(failed)) {
    kwb.utils::stopFormatted(
      "Download for all %d measurement chains files failed!",
      length(sftp_paths))
  }
  
  sapply(sftp_paths[!failed], function(sftp_path) {
    fs::path_join(parts = c(target_directory, sftp_path))
  })
}

#' Measurement Chains: read csv data 
#'
#' @param csv_paths character vector with paths to downloaded csv files. As 
#' retrieved by \code{\link{download_measurementchains_data}} 
#' @param debug show debug messages (default: FALSE)
#' @return data frame with imported data from csv files
#' @export
#' @importFrom readr read_csv col_datetime
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom tidyselect all_of
#' @examples 
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' target_directory <- tempdir()
#' csv_paths <- kwb.geosalz::download_measurementchains_data(
#' sftp_paths = mc_files$sftp_path,
#' target_directory)
#' mc_data <- kwb.geosalz::read_measurementchains_data(csv_paths)
#' 
#' }
read_measurementchains_data <- function(csv_paths, debug = FALSE)
{
  data_list <- lapply(csv_paths, function(csv_path) {
    readr::read_csv(
      file = csv_path,
      id = "csv_path",
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
    )
  })
  
  dplyr::bind_rows(data_list) %>% 
    dplyr::rename(
      sensor_id = .data$Geraet,
      datum_uhrzeit = .data$DatumUhrzeit
    ) %>% 
    dplyr::mutate(
      sensor_endnummer = stringr::str_extract(.data$sensor_id, "[0-9]$") %>% 
        as.integer()) %>% 
    tidyr::pivot_longer(
      names_to = "parameter", 
      values_to = "messwert", 
      cols = tidyselect::all_of(c("Leitfaehigkeit", "Temperatur"))
    )
}
