#' Helper function: get_excelcnv_exe
#'
#' @param office_folder office folder path (default: \code{safe_office_folder}
#'
#' @return path containing 'excelcnv.exe'
#' @importFrom kwb.utils collapsed
#' @export

get_excelcnv_exe <- function(office_folder = safe_office_folder()) {
  x <- office_folder

  paths <- list.files(x, "excelcnv\\.exe$", recursive = TRUE, full.names = TRUE)

  if ((n <- length(paths)) == 0) {
    stop("Excel conversion tool 'excelcnv.exe' not found under: ", x)
  }

  if (n > 1) {

    # Sort (in case of multiple executables newest is at index 1)
    paths <- sort(paths, decreasing = TRUE)

    cat(paste0(
      "\nFound ", n, " versions of 'excelcnv.exe':\n  ",
      kwb.utils::collapsed(paths, "\n  "),
      "\n\nUsing the latest one:\n  ", paths[1], "\n\n"
    ))
  }

  paths[1]
}

#' Helper function: safe_office_folder
#' @param office_path office folder path (default: 
#' "C:/Program Files (x86)/Microsoft Office")
#' @return path of office folder (if existing)
#' @importFrom kwb.utils safePath
safe_office_folder <- function(
  office_path = "C:/Program Files (x86)/Microsoft Office") {
  kwb.utils::safePath(office_path)
}


#' Helper function: delete_registry
#'
#' @param office_folder office folder path (default: \code{safe_office_folder})
#' @param dbg debug (default: TRUE)
#' @importFrom kwb.utils catIf
#' 
delete_registry <- function(office_folder = safe_office_folder(), dbg = TRUE) {
  exe_path <- get_excelcnv_exe(office_folder)

  parent_folder <- basename(dirname(exe_path))

  # Delete registry entry:
  # http://justgeeks.blogspot.com/2014/08/
  # free-convert-for-excel-files-xls-to-xlsx.html

  patterns <- kwb.utils::resolve(list(
    office = "HKEY_CURRENT_USER\\Software\\Microsoft\\Office",
    reg_entry = "<office>\\<version>.0\\Excel\\Resiliency\\StartupItems",
    command = "reg delete <reg_entry> /f",
    debug = "\nDeleting registry entry:\n<command>\n",
    version = stringr::str_extract(parent_folder, pattern = "[1][0-9]")
  ))

  kwb.utils::catIf(dbg, patterns$debug)

  system(command = patterns$command)
}


#' Convert xls to xlsx
#' @param input_dir input directory containing .xls files
#' @param export_dir export directory (default: tempdir())
#' @param office_folder office folder path (default: \code{safe_office_folder})
#' @param dbg debug (default: TRUE)
#' @importFrom  fs dir_create
#' @export
#'
convert_xls_as_xlsx <- function(input_dir,
                                export_dir = tempdir(), 
                                office_folder = safe_office_folder(), 
                                dbg = TRUE) {
  input_dir <- normalizePath(input_dir)

  export_dir <- normalizePath(export_dir)

  pattern <- "\\.([xX][lL][sS])$"

  xls <- normalizePath(dir(
    input_dir, pattern,
    recursive = TRUE, full.names = TRUE
  ))

  xlsx <- gsub(input_dir, export_dir, xls, fixed = TRUE)

  xlsx <- gsub(pattern, ".xlsx", xlsx)

  fs::dir_create(path = normalizePath(dirname(xlsx)), recursive = TRUE)

  exe <- normalizePath(get_excelcnv_exe(office_folder))

  for (i in seq_along(xls)) {
    convert_xls_to_xlsx(exe, xls[i], xlsx[i], i, length(xls), dbg = dbg)

    delete_registry(office_folder, dbg = dbg)
  }
}

#' Helper function: convert_xls_to_xlsx
#'
#' @param exe exe 
#' @param xls xls
#' @param xlsx xlsx
#' @param i i
#' @param n_files n_files 
#' @param dbg debug (default: TRUE)
#' @importFrom kwb.utils catIf


convert_xls_to_xlsx <- function(exe, xls, xlsx, i, n_files, dbg = TRUE) {
  command <- sprintf('"%s" -oice "%s" "%s"', exe, xls, xlsx)

  kwb.utils::catIf(dbg, sprintf(
    "\nConverting xls to xlsx (%d/%d):\n%s\n", i, n_files, command
  ))

  system(command)
}

###############################################################################
### Uncomment functions created just for testing DRAKE
###############################################################################

# get_file_paths_with_pattern <- function(input_dir, 
#                       pattern = "\\.([xX][lL][sS])$") {
#   input_dir <- normalizePath(input_dir)
#   
# 
#   
#   normalizePath(dir(
#     input_dir, pattern,
#     recursive = TRUE, full.names = TRUE
#   ))
#   
#   
# }


# get_xls_file_paths <- function(input_dir, pattern = "\\.([xX][lL][sS])$") {
#   get_file_paths_with_pattern(input_dir, pattern)
# }
# 
# get_xlsx_file_paths <- function(input_dir, pattern = "\\.([xX][lL][sS][xX])$") {
#   get_file_paths_with_pattern(input_dir, pattern)
# }
# 
# convert_xls_as_xlsx2 <- function(xls_paths,
#   input_dir,
#   export_dir, 
#   office_folder = safe_office_folder(), 
#   dbg = TRUE) {
#   input_dir <- normalizePath(input_dir)
#   
#   export_dir <- normalizePath(export_dir)
#   
#   
#   xlsx <- gsub(input_dir, export_dir, xls_paths, fixed = TRUE)
#   
#   xlsx <- gsub("\\.([xX][lL][sS])$", ".xlsx", xlsx)
#   
#   fs::dir_create(path = normalizePath(dirname(xlsx)), recursive = TRUE)
#   
#   exe <- normalizePath(get_excelcnv_exe(office_folder))
#   
#   for (i in seq_along(xls_paths)) {
#     convert_xls_to_xlsx(exe, xls_paths[i], xlsx[i], i, length(xls_paths), dbg = dbg)
#     
#     delete_registry(office_folder, dbg = dbg)
#   }
# }