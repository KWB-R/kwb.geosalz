#' Helper function: get_site_id
#'
#' @param string vector with character strings
#' @param pattern pattern used for identifying site_id (default: "^[0-9]{1,4}")
#' @return extracted site_id`s from input string
#' @importFrom stringr str_extract
#' @export
get_site_id <- function(string, pattern = "^[0-9]{1,4}")
{
  as.numeric(stringr::str_extract(string, pattern))
}

#' Helper function: gather_ignore
#'
#' @param fields column names to be ignored for gathering (default: c(
#' "Datum", "KN", "[iI]nterne Nr.", "Name der", "Ort", "Probe", "Pr\\u00FC",
#' "Untersuchung", "Labor", "Jahr", "Galer", "Detail", "Me\\u00DF", "Zeit",
#' "Bezei", "Monat")
#' @return vector with ignored columns for gathering
#' @importFrom kwb.utils collapsed
#' @export
#'
gather_ignore <- function(
    fields = c(
      "Datum", "KN", "[iI]nterne Nr.", "Name der", "Ort", "Probe",
      "Pr\u00FC", "Untersuchung", "Labor", "Jahr", "Galer", "Detail",
      "Me\u00DF", "Zeit", "Bezei", "Monat"
    )
)
{
  or_pattern(fields)
}

#' Helper function: gather_ignore_clean
#'
#' @param fields column names to be ignored for gathering (default:
#' c("LabSampleCode", "Date", "Time", "Waterbody", "ExSiteCode", Site")
#' @return vector with ignored columns for gathering
#' @importFrom kwb.utils collapsed
#' @export
#'
gather_ignore_clean <- function(
    fields = c(
      "LabSampleCode", "Date", "Time", "Waterbody", "ExSiteCode", "Site"
    )
)
{
  or_pattern(fields)
}
