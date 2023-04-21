#' Helper function: stop if duplicated sample ids are found
#'
#' @param df data frame with samples in wide format
#' @param col_sampleid column name of sample id
#' @param path path to file from which \code{df} was read (for information only)
#' @param sheet optional in case EXCEL is used (default: "")
#'
#' @return error in case duplicated samples were found
#' @export
#'
#' @importFrom kwb.utils isNullOrEmpty selectColumns stopFormatted
stop_if_duplicated_samples_found <- function(df, col_sampleid, path, sheet = "")
{
  sample_ids <- kwb.utils::selectColumns(df, col_sampleid)
  
  is_duplicated <- duplicated(sample_ids)
  
  if (any(is_duplicated)) {
    
    kwb.utils::stopFormatted(
      paste(
        "The following sample ids in column `%s` %s\nin file '%s' are", 
        "duplicated:\n\n%s\n\nPlease delete them!"
      ),
      col_sampleid, 
      if (kwb.utils::isNullOrEmpty(sheet)) "" else sprintf("in '%s'", sheet),
      path,
      paste(sample_ids[is_duplicated], collapse = "\n")
    )
  }
}
