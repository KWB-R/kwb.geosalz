#' Measurement Chains: write csv data
#'
#' @param mc_data measurement chains data as retrieved by 
#' \code{\link{read_measurementchains_data}}
#' @param target_directory target directory
#' @param to_zip should data be zipped? (default: FALSE), if TRUE only a 
#' temporary csv file is created which will be subsequently zipped and deleted
#' @param debug print debug messages (default: FALSE)
#'
#' @return writes csv data to path
#' @export
#'
#' @importFrom archive archive_write_files
#' @importFrom dplyr n summarise 
#' @importFrom kwb.utils catAndRun replaceFileExtension
#' @importFrom fs dir_create file_delete path_abs
#' @importFrom readr write_csv
#' @importFrom stringr str_c str_remove_all str_replace
#' @importFrom withr with_dir

write_measurementchains_data <- function(
    mc_data,
    target_directory, 
    to_zip = FALSE,
    debug = FALSE
) 
{
  
  fs::dir_create(target_directory)
  
  mc_data_stats <- get_measurmentchains_data_stats(mc_data)
  
  datetime_to_character <- function(datetime) {
    stopifnot(length(datetime) == 1L)
    paste0(
      as.character(datetime) %>%  
        stringr::str_remove_all("[-:]") %>% 
        stringr::str_replace(" ", "-") %>% 
        stringr::str_c("TZ"),
      ifelse(attr(datetime, "tzone") == "Etc/GMT-1", "+01", "not-implemented")
    )
  }
  
  dataset_name <- deparse(substitute(expr = mc_data))
  
  csv_path <- sprintf(
    "%s/mc_data_%s_%s.csv",
    fs::path_abs(target_directory),
    datetime_to_character(min(mc_data_stats$datetime_min)),
    datetime_to_character(min(mc_data_stats$datetime_max))
  )
  
  msg <- sprintf(
    "Exporting provided dataset '%s' to '%s'",
    dataset_name, 
    csv_path
  )
  
  kwb.utils::catAndRun(
    messageText = msg, 
    expr =  {
      readr::write_csv(mc_data, file = csv_path)
    },
    dbg = debug
  )
  
  path <- csv_path
  
  if (to_zip) {
    
    zip_path <- file.path(target_directory, "mc_data.zip")
    
    msg <- sprintf(
      "Exporting provided dataset '%s' to '%s' and delete intermediate '%s'",
      dataset_name, 
      zip_path, 
      csv_path
    )
    
    kwb.utils::catAndRun(
      messageText = msg, 
      expr =  {
        withr::with_dir(target_directory, 
                        code = {
        archive::archive_write_files( 
          archive = zip_path,
          files = basename(csv_path))
        fs::file_delete(path = csv_path)
        path <- zip_path
      })},
      dbg = debug
    )
  }
  
  path
}
