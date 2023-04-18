# get_measurement_chain_data_on_cloud ------------------------------------------

#' Get Measurement Chain Data on KWB Cloud
#' 
#' @return data frame with the content of "mc_data.zip" in the GeoSalz project
#'   folder on the Nextcloud server. The SFTP paths to the files from which the
#'   data in "mc_data.zip" originate are returned in attribute "sftp_paths".
#'   If either of the files "mc_data.zip" or "mc_files.csv" does not exist, 
#'   \code{NULL} is returned.
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.nextcloud list_files
#' @export
get_measurement_chain_data_on_cloud <- function()
{
  path <- "projects/GeoSalz/Monitoring/messketten"
  suppressMessages(info <- kwb.nextcloud::list_files(path, full_info = TRUE))
  
  # Helper function to get href for a given file name
  get_href <- function(file_name) {
    is_match <- kwb.utils::selectColumns(info, "file") == file_name
    if (any(is_match)) {
      kwb.utils::selectColumns(info, "href")[is_match]
    } else {
      ""
    }
  }

  # Try to get hrefs for files to be downloaded
  href_data <- get_href("mc_data.zip")
  href_files <- get_href("mc_files.csv")
  
  # Return NULL if any of the two required files is not available
  if (href_data == "" || href_files == "") {
    return(NULL)
  }
  
  # Download the two files
  files <- kwb.nextcloud::download_files(hrefs = c(href_data, href_files))

  # Extract the zip file
  zip_file <- files[1L]
  exdir <- dirname(zip_file)
  unzip(zip_file, exdir = exdir)
  
  # Get the full path to the extracted file
  data_file <- dir(exdir, "mc_data.*\\.csv$", full.names = TRUE)
  
  # Read the data from the unzipped csv file
  data <- read.csv(data_file)
  
  # Convert the date time character to posix
  data[["datum_uhrzeit"]] <- kwb.utils::selectColumns(data, "datum_uhrzeit") %>%
    utc_text_to_posix_gmt_plus_1()
  
  # Read information on the source files from which data originates
  file_info <- read.csv(files[2L])
  
  # Return the data with the names of the source files in attribute "files"
  structure(
    data, 
    sftp_paths = kwb.utils::selectColumns(file_info, "sftp_path")
  )
}

# utc_text_to_posix_gmt_plus_1 -------------------------------------------------
utc_text_to_posix_gmt_plus_1 <- function(x)
{
  # The given vector must be of type character
  stopifnot(is.character(x))
  
  # All elements in x must look like this:
  # <year>-<month>-<day>T<hour><minute><second>Z
  stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)))
  
  structure(
    as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    tzone = "Etc/GMT-1"
  )
}
