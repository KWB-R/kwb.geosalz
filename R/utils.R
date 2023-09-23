# all_defined ------------------------------------------------------------------

#' Check if all strings are not empty
#' 
#' @param x vector of character
#' @return \code{TRUE} or \code{FALSE}
all_defined <- function(x)
{
  !any(is_empty_string(x))
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
