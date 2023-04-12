#' Emshoff 91: list to data frame
#'
#' @param emshoff91_list  list as retrieved by \code{\link{read_multiple_emshoff91_ods}}
#' @return tibble
#' @export
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble

emshoff91_list_to_df <- function(emshoff91_list)
{
  emshoff91_list %>%
    data.table::rbindlist(
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
#' ku_sto = "kupp_st", lf = "el_lf", progr = "beprob_progr", strat = "stratigr", 
#' uv254 = "uv_ext"))
#' @param delete_cols should unneeded columns be deleted, i.e. the ones where data 
#' where mapped from (default: TRUE)
#' @return data frame with remapped values and deleted columns were this values 
#' were copied from (default: TRUE)
#' @export
#' @importFrom kwb.utils catAndRun
emshoff91_remap_values <- function(
    emshoff91_df, 
    remap_list = list(
      fi_mi = "fi_mi_m_nn",
      ku_sto = "kupp_st",
      lf = "el_lf", 
      progr = "beprob_progr",
      strat = "stratigr",
      uv254 = "uv_ext"
    ),
    delete_cols = TRUE
)
{
  tmp <- emshoff91_df
  
  for (i in seq_along(remap_list)) {
    
    col_old <- names(remap_list[i])
    col_new <- remap_list[[i]]
    
    to_replace <- is.na(tmp[[col_old]]) & !is.na(tmp[[col_new]])

    # Continue with the next iteration if there are no rows to replace    
    if (!any(to_replace)) {
      next
    }
    
    kwb.utils::catAndRun(
      messageText = sprintf(
        paste0(
          "Replacing %d empty rows in column '%s' with values in ", 
          "column '%s'. Deleting unneeded column '%s'!"
        ),
        sum(to_replace), 
        col_old, 
        col_new,
        col_new
      ), 
      expr = {
        tmp[[col_old]][to_replace] <- tmp[[col_new]][to_replace]
        if (delete_cols) {
          tmp <- dplyr::select(tmp, - .data[[col_new]])
        }
      }
    )
  }
  
  tmp
}
