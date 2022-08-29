#' Emshoff 91: list to data frame
#'
#' @param emshoff91_df  tibble as retrieved by \code{\link{read_multiple_emshoff91_ods }}
#' @return tibble
#' @export
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble

emshoff91_list_to_df <- function(emshoff91_list) {

  data.table::rbindlist(emshoff91_list, 
                        idcol = "ods_names_clean", 
                        fill = TRUE
                        ) %>% 
  tibble::as_tibble()
}

#' Emshoff 91: remap values from imported tibble
#'
#' @param emshoff91_df  tibble as retrieved by \code{\link{emshoff91_list_to_df}}
#' @param remap_list list with values to be remapped. Names of the list are columns 
#' values contained in list values should be mapped to (default: list(fi_mi = "fi_mi_m_nn", 
#' ku_sto = "kupp_st", lf = "el_lf", strat = "stratigr", uv254 = "uv_ext")), 
#' @return data frame with remapped values and deleted columns were this values 
#' were copied from
#' @export
#' @importFrom kwb.utils catAndRun
emshoff91_remap_values <- function(emshoff91_df, 
                                    remap_list = list(fi_mi = "fi_mi_m_nn",
                                                      ku_sto = "kupp_st",
                                                      lf = "el_lf", 
                                                      strat = "stratigr",
                                                      uv254 = "uv_ext"
                                    )
) {
  
  tmp <- emshoff91_df

sapply(seq_len(length(remap_list)),
       function(i) {
col_old <- names(remap_list[i])
col_new <- remap_list[[i]]
rows_to_replace <- which(is.na(tmp[,col_old]) & !is.na(tmp[, col_new]))
if(length(rows_to_replace) > 0) {
  msg <- sprintf(paste0("Replacing %d empty rows in column '%s' with values in ", 
                        "column '%s'. Deleting unneeded column '%s'!"),
                 length(rows_to_replace), 
                 col_old, 
                 col_new,
                 col_new)
  kwb.utils::catAndRun(msg, expr = {
  tmp[rows_to_replace,col_old] <- tmp[rows_to_replace,col_new] 
  tmp <- tmp %>% 
    dplyr::select(- .data[[col_new]])
  })
}})

tmp
}


