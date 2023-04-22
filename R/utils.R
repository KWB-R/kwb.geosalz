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

# get_environment_variables ----------------------------------------------------
get_environment_variables <- function(...) 
{
  lapply(list(...), Sys.getenv)
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
