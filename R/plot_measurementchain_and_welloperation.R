#' Plot measurementchain and well operation in combined plot
#'
#' @param mc_dat mc_dat 
#' @param well_op_data_meta well_op_data_meta 
#' @param brunnen_nr well id (default: 9)
#' @param para parameter (either: "Leitfaehigkeit" or "Temperatur")
#' @param y_label y label (default: "elektr. Leitfaehigkeit (µS/cm)")
#' @param date_min minimum date for plotting (default: as.Date("2023-05-10"))
#' @param date_max maximum date for plotting (default: Sys.Date())
#' @return combined plot
#' @export
#' @importFrom dplyr filter group_by summarize n 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_bw
#' theme guides guide_legend element_blank geom_bar scale_x_date
#' @importFrom zoo rollmean
plot_measurementchain_and_well_operation <- function(mc_dat,
                                                     well_op_data_meta,
                                                     brunnen_nr = 9,
                                                     para = "Leitfaehigkeit",
                                                     y_label = "elektr. Leitf\u00E4higkeit (\u00B5S/cm)",
                                                     date_min = as.Date("2023-05-10"), 
                                                     date_max = Sys.Date()) {
  
  well_ids <- c(9,10,13)
  
  if (! brunnen_nr %in% well_ids) {
    stop("'brunnen_nr' has to be one of: ", paste(well_ids, collapse = ", "))
  }

# plot time series Brunnen 9
selection <- mc_dat %>% 
  dplyr::filter(.data[["parameter"]] ==  para, 
                .data[["brunnen_nummer"]] == brunnen_nr)


n_sensors <- length(unique(selection$einbau_sensor_muGOK))
                    
custom_palette <- RColorBrewer::brewer.pal(n_sensors, 
                                           "Dark2")

p_well <- ggplot2::ggplot(selection,
                          ggplot2::aes(x = datum_uhrzeit,
                                       y = messwert,
                                       group = einbau_sensor_muGOK,
                                       color = as.factor(einbau_sensor_muGOK))) +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = custom_palette) + 
  ggplot2::labs(x="", y = y_label, color = "Sensor [muGOK]") +
  ggplot2::theme_bw() +
  ggplot2::xlim(as.POSIXct(date_min), as.POSIXct(date_max))  +
  #ggplot2::ylim(500,3000) +
  ggplot2::theme(legend.position = "top",
                 axis.text.x = ggplot2::element_blank()) +  
  ggplot2::guides(color = ggplot2::guide_legend(ncol = n_sensors)) 

#p_well

dat_well <- well_op_data_meta %>% dplyr::filter(.data$brunnen_nummer == brunnen_nr)

sum_well <- dat_well %>%
  dplyr::group_by(.data$bwb_datum) %>% 
  dplyr::summarise(n = dplyr::n(), 
            total_q = sum(.data$menge_summe_m3, na.rm = TRUE) )

sum_well$ma7 <- zoo::rollmean(sum_well$total_q, k = 7, fill = NA, align = "right")
sum_well$ma10 <- zoo::rollmean(sum_well$total_q, k = 10, fill = NA, align = "right")

plot_q_well <- ggplot2::ggplot(sum_well, ggplot2::aes(x = as.Date(bwb_datum), 
                                                      y = total_q)) +
  ggplot2::geom_bar(stat = "identity", width=1, color = "blue") +
  ggplot2::labs(x="", y = sprintf("Q, Brunnen %2d (m3/d)", brunnen_nr)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank()) + 
  ggplot2::xlim(date_min, date_max) 
  # ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, 
  #                                                    vjust = 0.5, 
  #                                                    hjust = 1)) #+


sum_wellfield <- well_op_data_meta %>%
  dplyr::group_by(.data$bwb_datum) %>% 
  dplyr::summarise(n = dplyr::n(), 
                   total_q = sum(.data$menge_summe_m3, na.rm = TRUE) )

sum_wellfield$ma7 <- zoo::rollmean(sum_well$total_q, k = 7, fill = NA, align = "right")
sum_wellfield$ma10 <- zoo::rollmean(sum_well$total_q, k = 10, fill = NA, align = "right")

plot_q_wellfield <- ggplot2::ggplot(sum_wellfield, ggplot2::aes(x = as.Date(bwb_datum), y = total_q)) +
  ggplot2::geom_bar(stat = "identity", width=1, color = "blue") +
  ggplot2::labs(x="Zeit", y = "Q, Brunnenfeld K-Galerie (m3/d)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
                        limits = c(date_min, date_max)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, 
                                                     vjust = 0.5, 
                                                     hjust = 1))

combined_plot <- cowplot::plot_grid(p_well, 
                                    plot_q_well,
                                    plot_q_wellfield, 
                                    ncol = 1, align = 'v')

combined_plot_with_title <- cowplot::ggdraw() +
  cowplot::draw_plot(combined_plot, 0, 0, 1, 1) +
  cowplot::draw_label(sprintf("Brunnen %2d", brunnen_nr), x = 0.2, y = 0.8, size = 12, hjust = 0.5)

combined_plot_with_title

}


