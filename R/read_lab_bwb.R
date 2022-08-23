#' Replace n.a. (not available) and n.b. (not determined) from lab data with NA
#' @param string string
#'
#' @return string with NA instead of "n.a." or "n.b." (including 0-10 spaces between 
#' "n" and "a"/"b"
#' @export
#'
#' @examples
#' string <- c("19.2", "n.b.", "n. b.", "n.  b.", "n.a.", "n. a.", "n.  a.")
#' replace_nanb_with_na(string)
#' @importFrom stringr str_replace

replace_nanb_with_na <- function(string) {

stringr::str_replace(string, 
                     pattern = "n\\.\\s{0,10}(a|b)\\.", 
                     replacement = NA_character_) 


}


#' Read Lab BWB
#'
#' @param path path to file with lab BWB data
#'
#' @return cleaned data frame with master data and lab values for all samples but 
#' only for selected parameters (columns A-BA and HB-HC)
#' @export
#'
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom dplyr bind_cols
#' @importFrom kwb.utils hsMatrixToListForm
#' @importFrom stringr str_detect str_replace
#' @importFrom kwb.base hsLabValToVal
#' @importFrom tidyr separate
#' @importFrom janitor clean_names
#' @importFrom dplyr filter if_else mutate relocate
#' @importFrom tibble as_tibble

read_lab_bwb <- function(path) {

sheets_needed <- c("Stammdaten", "Analysen")

sheets_available <- readxl::excel_sheets(path)

is_available <- sheets_needed %in% sheets_available
if(!all(is_available)) {
  msg <- sprintf("The following sheets are not available the Excel file %s:\n%s",
                 path,
                 paste0("'", sheets_needed[!is_available], "'", collapse = ", "))
  stop(msg)
}



master_data <- readxl::read_xlsx(path,
                                 sheet = "Stammdaten") %>% 
  janitor::clean_names()

rows_to_skip <- 5

lab_bwb_01 <- readxl::read_xlsx(path = path,
                             sheet = "Analysen",
                             range = cellranger::cell_limits(ul = c(rows_to_skip,1), 
                                                             lr = c(NA, 53)))

lab_bwb_02 <- readxl::read_xlsx(path = path,
                                sheet = "Analysen",
                                range = cellranger::cell_limits(ul = c(rows_to_skip,210), 
                                                                lr = c(NA, 211)))

lab_bwb <- dplyr::bind_cols(lab_bwb_01, lab_bwb_02) 


# lab_bwb <- readxl::read_xlsx(path = path,
#                              sheet = "Analysen",
#                              range = cellranger::cell_limits(ul = c(5,1), 
#                                                              lr = c(NA, 211)))

keyFields <- grep("@", names(lab_bwb), invert = TRUE, value = TRUE)
result <- kwb.utils::hsMatrixToListForm(as.data.frame(lab_bwb), keyFields,
                                        colNamePar = "key", 
                                        colNameVal = "par_val_org") %>%  
  tidyr::separate(col = "key", 
                  into = c("par_name", "method", "par_name_phreeqc", "unit_org"),
                  sep = "@") %>% 
  janitor::clean_names()

result$par_val <- replace_nanb_with_na(result$par_val_org) 


idx_below_or_above_detection_limit <- which(stringr::str_detect(result$par_val,
                                                                pattern = "<|>"))  


idx_style_german <- idx_below_or_above_detection_limit[which(stringr::str_detect(result$par_val[idx_below_or_above_detection_limit], 
                                        pattern = ","))]

if (length(idx_style_german) > 0) {
  result$parVal[idx_style_german] <- stringr::str_replace(result$par_val[idx_style_german], ",", ".")
}


idx_style_english <- idx_below_or_above_detection_limit[which(stringr::str_detect(result$par_val[idx_below_or_above_detection_limit], 
                                                                                 pattern = "."))]


stopifnot(length(idx_style_english) == length(idx_below_or_above_detection_limit))

result <- result %>%  
  dplyr::bind_cols(kwb.base::hsLabValToVal(result$par_val_org,
                                           country = "en",
                                           detLimFactorBelow = 0.5,
                                           stopOnError = FALSE)
                   ) %>%
  janitor::clean_names() %>% 
  dplyr::mutate(unit = dplyr::if_else(condition = .data$unit_org == "\u00B5g/l",
                                      true = "mg/l",
                                      false = .data$unit_org),
                numeric_value = dplyr::if_else(condition = .data$unit_org == "\u00B5g/l",
                                               true = .data$numeric_value * 0.001,
                                               false = .data$numeric_value)
                ) 

character_paras <- c("Farbe", "Tr\u00FCbung", "Geruch", "Bodensatz")

result <- result %>% 
  dplyr::filter(.data$par_name %in% character_paras & !is.na(.data$par_val_org) |
                !.data$par_name %in% character_paras & !is.na(.data$numeric_value)) %>% 
  dplyr::mutate(character_value = dplyr::if_else(.data$par_name %in% character_paras,
                                                 true = .data$par_val_org, 
                                                 false = NA_character_)
                ) %>% 
  dplyr::relocate(.data$character_value, .before = .data$numeric_value) %>% 
  tibble::as_tibble() %>% 
  dplyr::left_join(master_data, by = c("messstelle" = "name"))


return(result)
}



