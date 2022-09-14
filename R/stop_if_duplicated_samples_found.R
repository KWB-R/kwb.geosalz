#' Helper function: stop if duplicated sample ids are found
#'
#' @param df data frame with samples in wide format
#' @param col_sampleid column name of sample id
#' @param sheet optional in case EXCEL is used (default: "")
#'
#' @return error in case duplicated samples were found
#' @export
#'
#' @importFrom kwb.utils isNullOrEmpty stopFormatted
stop_if_duplicated_samples_found <- function(df, col_sampleid, sheet = "")
{
  is_duplicated_sampleid <- duplicated(df[[col_sampleid]])
  
  if (any(is_duplicated_sampleid)) {
    
    kwb.utils::stopFormatted(
      paste(
        "The following sample ids in column `%s` %s\nin file '%s' are", 
        "duplicated:\n\n%s\n\nPlease delete them!"
      ),
      col_sampleid, 
      ifelse(kwb.utils::isNullOrEmpty(sheet), "", sprintf("in '%s'", sheet)),
      path,
      paste(df[[col_sampleid]][is_duplicated_sampleid], collapse = "\n")
    )
  }
}
