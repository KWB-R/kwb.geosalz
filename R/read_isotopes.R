#' Read Isotopes
#'
#' @param path path to Isotopes delim (field separator ";")
#'
#' @return imported isotopes data
#' @export
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom stringr str_replace
read_isotopes <- function(path) {
  
  isotopes <- readr::read_delim(path, col_types = "ccdddcd", delim = ";") %>% 
  janitor::clean_names() 
  ### samples taken by KWB
  ### lab: UFZ
  # to to: fix date, add metadata join with BWB lab data
  
  names(isotopes) <- stringr::str_replace(names(isotopes),
                                          pattern = "^x",
                                          replacement = "isotope_")  
  isotopes
}
