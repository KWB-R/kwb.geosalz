meta_mess <- readr::read_csv(
  file = system.file("extdata/metadata_messketten.csv",
                     package = "kwb.geosalz"),
  show_col_types = FALSE
)


mk <- list(server = Sys.getenv("MESSKETTEN_SERVER"),
     user = Sys.getenv("MESSKETTEN_USER"),
     pw = Sys.getenv("MESSKETTEN_PASSWORD")
)

con <- sftp::sftp_connect(server = mk$server,
                          username = mk$user,
                          password = mk$pw)

files <- sftp::sftp_list(con,
                         recurse = TRUE)

library(kwb.geosalz)
files <- files %>%
  dplyr::filter(.data$type == "file")

tdir <- tempdir()

sftp::sftp_download(file = files$name,
                    sftp_connection = con,
                    tofolder = tdir)
