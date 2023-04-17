# get_measurement_chain_data_on_cloud ------------------------------------------

#' Get Measurement Chain Data on KWB Cloud
#' 
#' @return data frame with the content of "mc_data.zip" in the GeoSalz project
#'   folder on the KWB nextcloud. The SFTP paths to the files from which the
#'   data in "mc_data.zip" originate are returned in attribute "sftp_paths".
#' @export
get_measurement_chain_data_on_cloud <- function()
{
  suppressMessages(
    file_info <- kwb.nextcloud::list_files(
      "projects/GeoSalz/Monitoring/messketten", 
      full_info = TRUE
    )
  )
  
  cloud_files <- kwb.utils::selectColumns(file_info, "file")
  cloud_hrefs <- kwb.utils::selectColumns(file_info, "href")

  local_files <- kwb.nextcloud::download_files(hrefs = c(
    cloud_hrefs[cloud_files == "mc_data.zip"],
    cloud_hrefs[cloud_files == "mc_files.csv"]
  ))

  # Extract the zip file and provide the path "data_file" to the unzipped file  
  zip_file <- local_files[1L]
  exdir <- dirname(zip_file)
  unzip(zip_file, exdir = exdir)
  data_file <- dir(exdir, "mc_data.*\\.csv$", full.names = TRUE)
  
  # Read the data from the unzipped csv file
  data <- read.csv(data_file)
  
  # Read the information on source files
  file_info <- read.csv(local_files[2L])
  
  # Return the data with the names of the source files in attribute "files"
  structure(
    data, 
    sftp_paths = kwb.utils::selectColumns(file_info, "sftp_path")
  )
}
