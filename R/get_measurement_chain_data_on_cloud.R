# get_measurement_chain_data_on_cloud ------------------------------------------

#' Get Measurement Chain Data on KWB Cloud
#' 
#' @param dbg logical indicating whether or not to show debug messages
#' @return data frame with the content of "mc_data.zip" in the GeoSalz project
#'   folder on the Nextcloud server. The SFTP paths to the files from which the
#'   data in "mc_data.zip" originate are returned in attribute "sftp_paths".
#'   If either of the files "mc_data.zip" or "mc_files.csv" does not exist, 
#'   \code{NULL} is returned.
#' @importFrom dplyr mutate
#' @importFrom kwb.nextcloud download_files list_files
#' @importFrom kwb.utils selectColumns
#' @export
get_measurement_chain_data_on_cloud <- function(dbg = TRUE)
{
  # Path to file on Nextcloud server
  path <- "projects/GeoSalz/Monitoring/messketten/mc_data.zip"
  
  # Return NULL if the file is not available
  if (!kwb.nextcloud::file_exists(path)) {
    return(NULL)
  }
  
  # Download the file, unzip the one and only (csv) file in the zip archive,
  # read it and convert the text timestamps to POSIXct
  kwb.nextcloud::download_files(paths = path) %>%
    unzip_first_file() %>%
    read.csv() %>%
    dplyr::mutate(
      # Convert the date time character to POSIXct
      datum_uhrzeit = utc_text_to_posix_gmt_plus_1(.data[["datum_uhrzeit"]])
    )
}

# unzip_first_file -------------------------------------------------------------
#' @importFrom kwb.utils safePath
#' @importFrom utils unzip
unzip_first_file <- function(file)
{
  # Get the name of the (only) file in the zip archive    
  filename <- unzip(file, list = TRUE)$Name[1L]
  
  # Set the extraction folder to the folder of the zip file
  exdir <- dirname(file)
  
  # Extract the zip file to the extraction folder
  utils::unzip(file, filename, exdir = exdir)
  
  # Return the full path to the unzipped file
  kwb.utils::safePath(exdir, filename)
}

# utc_text_to_posix_gmt_plus_1 -------------------------------------------------
utc_text_to_posix_gmt_plus_1 <- function(x)
{
  # The given vector must be of type character
  stopifnot(is.character(x))
  
  # All elements in x must look like this:
  # <year>-<month>-<day>T<hour><minute><second>Z
  stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)))
 
  times <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  structure(times, tzone = "Etc/GMT-1")
}
