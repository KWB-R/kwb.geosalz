#' Get PhreeqC data
#'
#' @param lab_bwb imported BWB lab data as retrieved by \code{\link{read_lab_bwb}}
#' @return tibble with columns solution, par_name_phreeqc (which are not empty or 
#' NA) and numeric_value
#' @export
#' @importFrom dplyr rename select  

get_phreeqc_data <- function(lab_bwb) {
  
  dat <- lab_bwb %>%
    dplyr::filter(!kwb.utils::isNaOrEmpty(.data$par_name_phreeqc))
  
  
  stopifnot(all(dat$unit[!dat$par_name_phreeqc %in% c("temp", "pH")] == "mg/l"))
  
  dat %>% 
    dplyr::select(.data$probe_nr, 
                  .data$par_name_phreeqc, 
                  .data$numeric_value)
}


#' Prepare PhreeqC input
#'
#' @param lab_bwb_phreeqc selected BWB lab data as retrieved by \code{\link{get_phreeqc_data}}
#' @param title user defined title (default: "Test Dataset") 
#' @return data frame with input structure for kwb.phreeqc
#' @export
#'
#' @importFrom kwb.utils isNaOrEmpty
#' @importFrom dplyr mutate select relocate rename 
#' @importFrom tidyr pivot_wider
#' @importFrom geosalz.phreeqc prepare_solutions_input tidy_samples
prepare_phreeqc_input <- function(lab_bwb_phreeqc,
                                  title = "Test Dataset") {
  

  lab_bwb_phreeqc %>% 
  dplyr::rename(solution = .data$probe_nr) %>% 
  dplyr::select(.data$solution, 
                .data$par_name_phreeqc, 
                .data$numeric_value) %>%
  tidyr::pivot_wider(names_from = .data$par_name_phreeqc, 
                      values_from = .data$numeric_value) %>% 
  dplyr::mutate(units = "ppm") %>% 
  dplyr::relocate(.data$units, .after = .data$solution) %>% 
  geosalz.phreeqc::tidy_samples() %>% 
  dplyr::mutate(solution = as.character(solution), 
                outOfLimit = "", 
                numericValue = as.numeric(.data$value)) %>% 
  dplyr::filter(!is.na(.data$numericValue)) %>% 
  geosalz.phreeqc::prepare_solutions_input(title = title)

} 

#' Convert PhreeqC input to "wide" format
#'
#' @param phreeqc_input PhreeqC input as retrieved by \code{\link{get_phreeqc_dat}}

#'
#' @return PhreeqC input in "wide" format
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rename
convert_phreeqc_input_to_wide <- function(phreeqc_input) {

phreeqc_input %>% 
    tidyr::pivot_wider(names_from = .data$par_name_phreeqc,
                       values_from = .data$numeric_value) %>%
    dplyr::rename(solution = .data$probe_nr)
}
