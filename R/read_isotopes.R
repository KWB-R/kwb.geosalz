#' Read Isotopes
#'
#' @param path path to Isotopes delim (field separator ";")
#'
#' @return imported isotopes data
#' @export
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom stringr str_replace
read_isotopes <- function(path)
{
  isotopes <- readr::read_delim(path, col_types = "ccdddcd", delim = ";") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(
      sampling_date = as.Date(.data$sampling_date, format = "%d.%m.%Y")
    ) %>% 
    dplyr::select(- .data$fuk_masl) %>% 
    dplyr::rename(
      probe_nr = .data$lab_id, 
      probenahme_datum = .data$sampling_date,
      messstelle = .data$site_id
    ) 
  
  names(isotopes) <- stringr::str_replace(
    names(isotopes),
    pattern = "^x",
    replacement = "isotope_"
  )
  
  stop_if_duplicated_samples_found(
    df = isotopes, 
    col_sampleid = "probe_nr", 
    path,
    sheet = ""
  )
  
  isotopes_long <- tidyr::pivot_longer(
    isotopes, 
    names_to = "par_name", 
    values_to = "par_val_org", 
    cols = tidyselect::contains(c("isotope", "cond"))
  )
  
  isotopes_long %>%
    dplyr::bind_cols(
      kwb.base::hsLabValToVal(isotopes_long$par_val_org, country = "en")
    ) %>% 
    ### samples taken by KWB
    ### lab: UFZ
    # to to: fix date, add metadata join with BWB lab data
    dplyr::mutate(
      probenahme = "KWB", 
      labor = "UFZ"
    ) %>% 
    dplyr::rename(
      numeric_value = .data$numericValue,
      out_of_limit = .data$outOfLimit
    )
}
