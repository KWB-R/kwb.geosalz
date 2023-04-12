#' Helper function: copy_lookup_para_file
#'
#' @param from_dir input directory with xlsx files
#' @param to_dir target directory where to copy the xlsx files
#' @param overwrite should existing files be overwritten (TRUE) otherwise (FALSE)
#' ? (default: FALSE)
#' @param recursive if TRUE recursively find all xlsx files in the directory
#' specified in parameter "from_dir" (default: TRUE)
#' @param file_pattern pattern for identifying lookup_para file
#' (default: "^lookup_para\\.csv$")
#' @importFrom fs dir_create file_copy
#' @importFrom kwb.utils stopFormatted
#' @export
copy_lookup_para_file <- function(
    from_dir,
    to_dir,
    overwrite = FALSE,
    recursive = TRUE,
    file_pattern = "^lookup_para\\.csv$"
)
{
  from_dir <- normalizePath(from_dir)
  to_dir <- normalizePath(to_dir)
  
  from_path <- normalizePath(dir(
    from_dir, 
    file_pattern,
    recursive = TRUE, 
    full.names = TRUE
  ))
  
  if (!file.exists(from_path)) {
    kwb.utils::stopFormatted(
      "No 'lookup_para' file found in the following input dir:\n%s",
      from_dir
    )
  }
  
  to_path <- gsub(from_dir, to_dir, from_path, fixed = TRUE)
  
  fs::dir_create(dirname(to_path), recursive = TRUE)
  
  cat(sprintf(
    "\nCopying 'lookup_para' file:\nFROM: %s\nTO  : %s\n",
    from_path, to_path
  ))
  
  fs::file_copy(from_path, to_path, overwrite = overwrite)
}
