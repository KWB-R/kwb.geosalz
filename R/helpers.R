#' Helper function: cat_green_bold_0
#' 
#' @param ... text passed to crayon::green()
#' @importFrom crayon bold
#' @importFrom crayon green
#' @return formatted text output
cat_green_bold_0 <- function(...)
{
  cat(crayon::green(crayon::bold(paste0(...))))
}

#' Helper function: cat_red_bold_0
#' 
#' @param ... text passed to crayon::red
#' @importFrom crayon bold
#' @importFrom crayon red
#' @return formatted text output
#' 
cat_red_bold_0 <- function(...) 
{
  cat(crayon::bold(crayon::red(paste0(...))))
}

#' Helper function: get_meta_sheet_or_stop
#'
#' @param sheets sheets
#' @param pattern pattern
#' @param file file
#' @importFrom kwb.utils stopFormatted
#' @return meta sheet name
#' 
get_meta_sheet_or_stop <- function(sheets, pattern, file)
{
  meta_sheets <- sheets[stringr::str_detect(sheets, pattern)]
  
  # Number of meta sheets
  n <- length(meta_sheets)
  
  if (n == 0L) {
    kwb.utils::stopFormatted(
      "%s does not contain a sheet matching '%s'\n",
      file, pattern
    )
  }
  
  if (n > 1L) {
    kwb.utils::stopFormatted(
      "%s contains %d sheets matching '%s': %s\n",
      file, n, pattern, kwb.utils::stringList(meta_sheets)
    )
  }
  
  meta_sheets
}

#' Helper function: get_site_id
#'
#' @param string vector with character strings
#' @param pattern pattern used for identifying site_id (default: "^[0-9]{1,4}")
#' @return extracted site_id`s from input string
#' @importFrom stringr str_extract
#' @export
#' 
get_site_id <- function(string, pattern = "^[0-9]{1,4}")
{
  as.numeric(stringr::str_extract(string, pattern))
}

#' Helper function: column_pattern_gather_ignore
#'
#' @param fields column names to be ignored for gathering (default: c(
#' "Datum", "KN", "[iI]nterne Nr.", "Name der", "Ort", "Probe", "Pr\\u00FC",
#' "Untersuchung", "Labor", "Jahr", "Galer", "Detail", "Me\\u00DF", "Zeit",
#' "Bezei", "Monat")
#' @return vector with ignored columns for gathering
#' @importFrom kwb.utils collapsed
#' @export
#' 
column_pattern_gather_ignore <- function(
    fields = c(
      "Datum", "KN", "[iI]nterne Nr.", "Name der", "Ort", "Probe",
      "Pr\u00FC", "Untersuchung", "Labor", "Jahr", "Galer", "Detail",
      "Me\u00DF", "Zeit", "Bezei", "Monat"
    )
)
{
  or_pattern(fields)
}

#' Helper function: column_pattern_gather_ignore_clean
#'
#' @param fields column names to be ignored for gathering (default:
#' c("LabSampleCode", "Date", "Time", "Waterbody", "ExSiteCode", Site")
#' @return vector with ignored columns for gathering
#' @importFrom kwb.utils collapsed
#' @export
#' 
column_pattern_gather_ignore_clean <- function(
    fields = c(
      "LabSampleCode", "Date", "Time", "Waterbody", "ExSiteCode", "Site"
    )
)
{
  or_pattern(fields)
}

#' Helper function: print_datatype_info_if
#'
#' @param dbg dbg
#' @param tbl_datatypes tbl_datatypes
#' @param columns_keep columns_keep
#' @importFrom kwb.utils stringList
#' @importFrom stringr str_c
#' 
print_datatype_info_if <- function(dbg, tbl_datatypes, columns_keep)
{
  if (!dbg) {
    return()
  }
  
  cat_green_bold_0(
    "The following datatypes were detected:\n",
    kwb.utils::stringList(qchar = "", sprintf(
      "%d x %s", as.numeric(tbl_datatypes), names(tbl_datatypes)
    ))
  )
  
  cat_green_bold_0(stringr::str_c(
    "\nThe following column(s) will be used as headers:\n",
    stringr::str_c(columns_keep, collapse = ", "), "\n"
  ))
}

#' Helper function: stop_on_missing_or_inform_on_extra_sheets
#'
#' @param has_site_id has_site_id
#' @param file file
#' @param sheets sheets
#' @importFrom kwb.utils stopFormatted stringList
#' 
stop_on_missing_or_inform_on_extra_sheets <- function(
    has_site_id, 
    file, 
    sheets
)
{
  if (!any(has_site_id)) {
    
    kwb.utils::stopFormatted(
      paste0(
        "No data sheet has a site code in its name!\n",
        "Folder:\n  %s\n",
        "File:\n  '%s'\n",
        "Sheet names:\n  %s\n"
      ),
      dirname(file), basename(file),
      kwb.utils::stringList(sheets, collapse = "\n  ")
    )
  }
  
  if (!all(has_site_id)) {
    
    crayon::blue(sprintf(
      "FROM: %s\nIgnoring the following (%d/%d) sheet(s):\n%s\n",
      file, sum(!has_site_id), length(sheets),
      kwb.utils::stringList(sheets[!has_site_id])
    ))
  }
}
