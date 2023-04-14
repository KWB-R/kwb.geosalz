#' Measurement Chains: plott
#'
#' @param mc_data as retrieved by \code{\link{read_measurementchains_data}}
#' @param para parameter to plot "Leitfaehigkeit" or "Temperatur" (default: 
#' Leitfaehigkeit")
#'
#' @return plot of selected measurement chain parameter
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes ggplot geom_line geom_point theme_bw labs
#' scale_color_discrete theme
#' @importFrom forcats fct_reorder
plot_measurementchains <- function(mc_data, para = "Leitfaehigkeit")
{
  unit <- ifelse(para == "Leitfaehigkeit", "\u00B5S/cm", "\u00B0C")
  
  metadata <- get_measurementchains_metadata()
  
  dat <- mc_data %>%
    dplyr::left_join(metadata, by = "sensor_id") 
  
  well_ids <- unique(dat$brunnen_nummer)
  
  titles_plot <- sprintf("Parameter: %s, Brunnen: K%d", para, well_ids)
  titles_list <- sprintf("%s_K%02d", para, well_ids)
  
  plots <- lapply(seq_along(well_ids), function(i) {
    dat %>% 
      dplyr::filter(.data$brunnen_nummer == well_ids[i]) %>% 
      dplyr::filter(
        .data$datum_uhrzeit >= as.POSIXct(
          "2022-09-27 11:00:00", 
          tz = "Etc/GMT-1"
        )
      ) %>% 
      dplyr::mutate(
        label = as.factor(sprintf(
          "%s m uGOK (%s)", 
          .data$einbau_sensor_muGOK, 
          .data$sensor_id
        )) %>%  
          forcats::fct_reorder(.data$einbau_sensor_muGOK)
      ) %>% 
      dplyr::filter(.data$parameter == para) %>%  
      ggplot2::ggplot(mapping = ggplot2::aes(
        x = .data$datum_uhrzeit, 
        y = .data$messwert, 
        col = .data$label
      )) +
      ggplot2::scale_color_discrete(name = "Einbautiefe (sensor_id)") +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = "",
        y = sprintf("Messwert (%s)", unit),
        title = titles_plot[i]
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")
  })
  
  stats::setNames(plots, titles_list)
}
