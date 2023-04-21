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

replace_nanb_with_na <- function(string)
{
  stringr::str_replace(
    string, 
    pattern = "n\\.\\s{0,10}(a|b)\\.", 
    replacement = NA_character_
  ) 
}

#' Read Master Data
#'
#' @param path path to file with master data (currently in file: lab BWB data)
#' @return imported master data contained in sheet "Stammdaten"
#' @export
#'
#' @importFrom kwb.utils stopFormatted stringList
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom janitor clean_names
read_master_data <- function(path)
{
  sheets_needed <- "Stammdaten"
  
  stop_on_missing_sheets(path, sheets_needed)
  
  readxl::read_xlsx(path, sheet = sheets_needed) %>%
    janitor::clean_names() %>% 
    dplyr::rename(messstelle = .data$name)
}  

stop_on_missing_sheets <- function(path, sheets_needed)
{
  not_available <- ! sheets_needed %in% readxl::excel_sheets(path)
  
  if (any(not_available)) {
    kwb.utils::stopFormatted(
      "The following sheets are not available the Excel file %s:\n%s",
      path,
      kwb.utils::stringList(sheets_needed[not_available])
    )
  }
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

read_lab_bwb <- function(path)
{
  sheets_needed <- c("Stammdaten", "Analysen")
  
  stop_on_missing_sheets(path, sheets_needed)
  
  rows_to_skip <- 5L
  
  lab_bwb_01 <- readxl::read_xlsx(
    path = path,
    sheet = "Analysen",
    range = cellranger::cell_limits(
      ul = c(rows_to_skip,1L), 
      lr = c(NA, 53L)
    )
  )
  
  lab_bwb_02 <- readxl::read_xlsx(
    path = path,
    sheet = "Analysen",
    range = cellranger::cell_limits(
      ul = c(rows_to_skip, 210L), 
      lr = c(NA, 211L)
    )
  )
  
  lab_bwb <- dplyr::bind_cols(lab_bwb_01, lab_bwb_02) 
  
  # lab_bwb <- readxl::read_xlsx(
  #   path = path,
  #   sheet = "Analysen",
  #   range = cellranger::cell_limits(ul = c(5L, 1L), lr = c(NA, 211L))
  # )
  
  stop_if_duplicated_samples_found(
    df = lab_bwb, 
    col_sampleid = "Probe-Nr.", 
    path,
    sheet = sheet
  )
  
  keyFields <- grep("@", names(lab_bwb), invert = TRUE, value = TRUE)
  
  result <- kwb.utils::hsMatrixToListForm(
    as.data.frame(lab_bwb), keyFields,
    colNamePar = "key", 
    colNameVal = "par_val_org"
  ) %>%  
    tidyr::separate(
      col = "key", 
      into = c("par_name", "method", "par_name_phreeqc", "unit_org"),
      sep = "@"
    ) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(probenahme_datum = as.Date(.data$probenahme_datum))
  
  result$par_val <- replace_nanb_with_na(result$par_val_org) 
  
  idx_below_or_above_detection_limit <- which(
    stringr::str_detect(result$par_val, pattern = "<|>")
  )
  
  idx_style_german <- idx_below_or_above_detection_limit[
    which(stringr::str_detect(
      result$par_val[idx_below_or_above_detection_limit], 
      pattern = ","
    ))
  ]
  
  if (length(idx_style_german) > 0L) {
    result$par_val[idx_style_german] <- stringr::str_replace(
      result$par_val[idx_style_german], ",", "."
    )
  }
  
  idx_style_english <- idx_below_or_above_detection_limit[
    which(stringr::str_detect(
      result$par_val[idx_below_or_above_detection_limit], 
      pattern = "."
    ))
  ]
  
  stopifnot(
    length(idx_style_english) == length(idx_below_or_above_detection_limit)
  )
  
  result <- result %>%  
    dplyr::bind_cols(kwb.base::hsLabValToVal(
      result$par_val,
      country = "en",
      detLimFactorBelow = 0.5,
      stopOnError = FALSE
    )) %>%
    janitor::clean_names() %>% 
    dplyr::mutate(
      unit = dplyr::if_else(
        condition = .data$unit_org == "\u00B5g/l",
        true = "mg/l",
        false = .data$unit_org
      ),
      numeric_value = dplyr::if_else(
        condition = .data$unit_org == "\u00B5g/l",
        true = .data$numeric_value * 0.001,
        false = .data$numeric_value
      )
    ) 
  
  character_paras <- c("Farbe", "Tr\u00FCbung", "Geruch", "Bodensatz")
  
  result %>% 
    dplyr::filter(
      .data$par_name %in% character_paras & !is.na(.data$par_val_org) |
        !.data$par_name %in% character_paras & !is.na(.data$numeric_value)
    ) %>% 
    dplyr::mutate(
      character_value = dplyr::if_else(
        .data$par_name %in% character_paras,
        true = .data$par_val_org, 
        false = NA_character_
      )
    ) %>% 
    dplyr::relocate(.data$character_value, .before = .data$numeric_value) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(labor = "BWB") %>% 
    dplyr::select(- .data$par_val)
}
