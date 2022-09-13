#' Measurement Chains: Get Metadata
#'
#' @param file path to measurement chains metadata file (default: 
#' system.file("extdata/metadata_messketten.csv", package = "kwb.geosalz"))
#' @return tibble with measurement chains metadata
#' @export
#'
#' @examples
#' mc_metadata <- kwb.geosalz::get_measurementchains_metadata()
#' str(mc_metadata)
get_measurementchains_metadata <- function(
    file = system.file("extdata/metadata_messketten.csv",
                       package = "kwb.geosalz")) {

readr::read_csv(
  file = file,
  show_col_types = FALSE
)
}

#' Measurement Chains: Create an SFTP Connection
#'
#' @return sftp connection
#' @export
#' @importFrom sftp sftp_connect
#' @importFrom stringr str_length
create_sftp_connection <- function()  {
  
  con_vars <- sprintf("MESSKETTEN_%s", c("SERVER", "USER", "PASSWORD"))
  con <- list(server = Sys.getenv(con_vars[1]),
       user = Sys.getenv(con_vars[2]),
       pw = Sys.getenv(con_vars[3])
       )
  
  is_defined <- sapply(con, stringr::str_length) > 0
  if(any(!is_defined)) {
    msg <- sprintf("The following required environment variables are undefined/empty:\n%s",
                   paste0(con_vars[!is_defined], collapse = ", "))
    stop(msg)
  }
  
  sftp::sftp_connect(server = con$server,
                     username = con$user,
                     password = con$pw)
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
#' @importFrom stringr str_extract str_remove
#' @examples 
#' \dontrun{
#' mc_files <- kwb.geosalz::get_measurementchains_files()
#' str(mc_files)
#' }
get_measurementchains_files <- function(sftp_connection = create_sftp_connection(),
                                       debug = FALSE) {
  

files <- sftp::sftp_list(sftp_connection,
                         recurse = TRUE,
                         verbose = debug) 

files %>%
  dplyr::filter(.data$type != "dir") %>% 
  tidyr::separate(.data$name,into = c("galerywellid", "deviceid_datetime"), sep = "/", 
                  remove = FALSE) %>%
  dplyr::rename(sftp_path = .data$name) %>% 
  dplyr::mutate(galerie = stringr::str_extract(.data$galerywellid, "^[A-Z]"),
                brunnen_nummer = stringr::str_extract(.data$galerywellid, "[0-9]{1,3}$") %>% 
                  as.numeric()) %>% 
  dplyr::mutate(deviceid_datetime = stringr::str_replace(.data$deviceid_datetime, 
                                                         pattern = "-202", "_202")) %>%
  tidyr::separate(.data$deviceid_datetime,into = c("deviceid", "datum_uhrzeit"), sep = "_") %>% 
  dplyr::mutate(endnummer_sensor = stringr::str_extract(.data$deviceid, "[0-9]$"),  
                datum_uhrzeit = stringr::str_remove(.data$datum_uhrzeit, "\\.csv$")) %>%  
  dplyr::mutate(datum_uhrzeit = as.POSIXct(.data$datum_uhrzeit, 
                                           format = "%Y-%m-%d-%H%M")) %>% 
  dplyr::select(- .data$galerywellid)
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
#' @importFrom kwb.utils catAndRun isTryError
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
download_measurementchains_data <- function(sftp_paths,
                                           target_directory = tempdir(),
                                           sftp_connection = create_sftp_connection(),
                                           run_parallel = TRUE,
                                           debug = FALSE) {

fs::dir_create(target_directory)
  
  msg <- sprintf(
    "Importing %d measurement chains files",
    length(sftp_paths)
  )  
  
  if (run_parallel) {
    
    ncores <- parallel::detectCores() - 1L
    
    cl <- parallel::makeCluster(ncores)
    
    dl_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = parallel::parLapply(
        cl, sftp_paths, function(sftp_path) {
          try(sftp::sftp_download(file = sftp_path,
                              sftp_connection = sftp_connection,
                              tofolder = target_directory, 
                              verbose = FALSE)
              )
        }),
      dbg = debug
    )
    
    parallel::stopCluster(cl)
    
  } else {
    
    dl_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = lapply(sftp_paths, function(sftp_path) {
      try(sftp::sftp_download(file = sftp_path,
                            sftp_connection = sftp_connection,
                            tofolder = target_directory, 
                            verbose = debug))}),
      dbg = debug)
    
  }
  
  failed <- sapply(dl_list, kwb.utils::isTryError)
  
  if (any(failed)) {
    message("Failed downloading data from the following FTP path(s):")
    message(paste0(sftp_paths[failed], collapse = "\n"))
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
#' @importFrom readr read_csv
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
read_measurementchains_data <- function(csv_paths,
                                       debug = FALSE) {
  
  
  
  data_list <- lapply(csv_paths, function(csv_path) {
         readr::read_csv(file = csv_path,
                         id = "csv_path",
                         show_col_types = debug)
  })
  
  
  dplyr::bind_rows(data_list) %>% 
  dplyr::mutate(endnummer_sensor = stringr::str_extract(.data$Geraet, "[0-9]$")) %>% 
  tidyr::pivot_longer(names_to = "parameter", 
                      values_to = "messwert", 
                      cols = tidyselect::all_of(c("Leitfaehigkeit", "Temperatur")))
}  
  

