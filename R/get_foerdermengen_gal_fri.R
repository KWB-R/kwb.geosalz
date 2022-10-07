#' Get Abtraction of Friedrichshagen Well Galleries
#'
#' @param path path to "2018-04-27 Rohwasser Bericht - Galeriefördermengen.xlsx"
#'
#' @return tidy data frame with abstraction rates for waterworks Friedrichshagen 
#' @export
#'
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_remove
#' @importFrom dplyr relocate mutate
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_xlsx
get_foerdermengen_gal_fri <- function(path)
{
  q_gal <- readxl::read_xlsx(
    path,
    sheet = "Gal. Jahresmengen",
    range = "A2:R70"
  )
  
  names(q_gal)[1] <- "year"
  
  werk <- "FRI"
  
  q_gal %>%
    dplyr::mutate(date = as.Date(sprintf("%s-12-31", .data$year))) %>%
    dplyr::relocate(.data$date, .after = .data$year) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(werk),
      names_to = "galerie",
      values_to = "foerdermenge_m3"
    ) %>%
    dplyr::mutate(
      galerie = stringr::str_remove(.data$galerie, pattern = "^FRI "),
      werk = werk
    )
}
