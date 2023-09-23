# all_defined ------------------------------------------------------------------

#' Check if all strings are not empty
#' 
#' @param x vector of character
#' @return \code{TRUE} or \code{FALSE}
all_defined <- function(x)
{
  !any(is_empty_string(x))
}

# as_gmt_plus_one --------------------------------------------------------------
as_gmt_plus_one <- function(x, format = "%Y-%m-%d %H:%M:%S")
{
  # data is always CET without switching
  # https://stackoverflow.com/a/38333522
  
  # Timezone string. GMT-1 is correct! the result will be GMT+1, e.g.
  # as_gmt_plus_one("2023-09-23 11:00:00") # "2023-09-23 11:00:00 +01"
  
  tzone <- "Etc/GMT-1"
  
  # If x is already a POSIXct object, change the tzone attribute
  if (inherits(x, "POSIXct")) {
    return(structure(x, tzone = tzone))
  } 
  
  # Otherwise we expect x to be of type character  
  stopifnot(is.character(x))
  
  # Convert character to POSIXct
  as.POSIXct(x, format = format, tz = tzone)
}

# as_utc -----------------------------------------------------------------------
as_utc <- function(x)
{
  # The given vector must be of type character
  stopifnot(is.character(x))
  
  # All elements in x must look like this:
  # <year>-<month>-<day>T<hour><minute><second>Z
  stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)))
  
  # Convert character to POSIXct
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# exclude_missing_files --------------------------------------------------------
exclude_missing_files <- function(files)
{
  is_missing <- !sapply(files, file.exists)
  
  if (any(is_missing)) {
    
    n_missing <- sum(is_missing)
    
    message(sprintf(
      "Excluding %d file%s that do%s not exist:\n- %s",
      n_missing,
      ifelse(n_missing > 1L, "s", ""),
      ifelse(n_missing > 1L, "", "es"),
      paste(files[is_missing], collapse = "\n- ")
    ))
    
    files <- files[!is_missing]
  }
  
  files
}

# extdata_file -----------------------------------------------------------------
extdata_file <- function(...)
{
  system.file("extdata", ..., package = "kwb.geosalz")
}

# get_environment_variables ----------------------------------------------------
get_environment_variables <- function(..., check. = FALSE)
{
  variables <- list(...)

  values <- lapply(variables, Sys.getenv)
  
  if (check. && any(is_empty <- is_empty_string(values))) {
    kwb.utils::stopFormatted(
      "The following required environment variables are undefined/empty:\n%s",
      paste0(unlist(variables[is_empty]), collapse = ", ")
    )
  }
  
  values  
}

# is_empty_string --------------------------------------------------------------
is_empty_string <- function(x)
{
  stringr::str_length(unlist(x)) == 0L
}

# or_pattern -------------------------------------------------------------------
or_pattern <- function(x)
{
  paste(x, collapse = "|")
}

# temp_dir ---------------------------------------------------------------------
temp_dir <- function(subfolder = "R_kwb.geosalz", dbg = TRUE)
{
  path <- Sys.getenv("TEMP", Sys.getenv("TMP", tempdir()))
  
  if (is.null(subfolder)) {
    return(path)
  }
  
  kwb.utils::createDirectory(file.path(path, subfolder), dbg = dbg)
}
