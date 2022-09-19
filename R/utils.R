# get_environment_variables ----------------------------------------------------
get_environment_variables <- function(...) 
{
  lapply(list(...), Sys.getenv)
}
