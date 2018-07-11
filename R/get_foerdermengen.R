#' get_foerdermengen
#'
#' @param xlsx_path path to xlsx file with pumping rates
#' @param sheet_name sheet_name (default: "WW Q Rhow ")
#' @param sheet_range sheet_range  (default: "A4:S127")
#' @return data frame with annual pumping rates per waterworks
#' @importFrom magrittr "%>%"
#' @importFrom readxl read_xlsx
#' @importFrom tidyr gather_
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_sub
#' @import dplyr
#' @export
get_foerdermengen <- function(xlsx_path,
                              sheet_name =  "WW Q Rhow ",
                              sheet_range = "A4:S127") {

  q_ww <- readxl::read_xlsx(
    xlsx_path, 
    sheet = sheet_name, 
    range = sheet_range) 
  
  q_ww <- q_ww %>% 
    tidyr::gather_(
      key_col = "Wasserwerk",
      value_col = "Foerdermenge_m3",
      gather_cols = setdiff(names(q_ww), "Jahr")
    ) %>%
    dplyr::rename_(year = "Jahr") %>%
    dplyr::filter_("!is.na(Foerdermenge_m3)")

  lookup_werk <- data.frame(
    Wasserwerk = unique(q_ww$Wasserwerk),
    werk = stringr::str_to_upper(
      stringr::str_sub(unique(q_ww$Wasserwerk), 1, 3)
    ),
    stringsAsFactors = FALSE
  )

  q_ww %>%
    dplyr::left_join(y = lookup_werk, by = "Wasserwerk")
}
