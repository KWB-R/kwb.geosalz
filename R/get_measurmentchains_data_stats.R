#' Measurement Chains: get statistics for data
#'
#' @param mc_data tibble with measurement chains data as retrieved by 
#' \code{\link{read_measurementchains_data}} 
#'
#' @return tibble with colunns datetime min/max, q10 (10% percentile), mean, 
#' median, q90 (90% percentile), max
#' @export
#'
#' @importFrom dplyr arrange group_by summarise
get_measurmentchains_data_stats <- function(mc_data) {
  
  mc_data %>%  
  dplyr::group_by(
    .data$sensor_id,
    .data$parameter
  ) %>% 
  dplyr::summarise(
    datetime_min = min(.data$datum_uhrzeit), 
    datetime_max = max(.data$datum_uhrzeit),
    min = min(.data$messwert, na.rm = TRUE), 
    q10 = quantile(.data$messwert, 0.1, na.rm = TRUE), 
    mean = mean(.data$messwert, na.rm = TRUE), 
    median = median(.data$messwert, na.rm = TRUE),
    q90 = quantile(.data$messwert, 0.9, na.rm = TRUE), 
    max = max(.data$messwert, na.rm = TRUE), 
    number_of_samples = dplyr::n()
  ) %>% 
  dplyr::arrange(
    .data$sensor_id, 
    .data$parameter
  )
}