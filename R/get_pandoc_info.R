#' Get Information on Pandoc
#' 
#' @return data frame with columns \code{pandoc_directory},
#'   \code{pandoc_version} if Pandoc is installed, otherwise a message is
#'   printed that pandoc is not installed.
#' @export
#' @importFrom rmarkdown pandoc_exec pandoc_version
get_pandoc_info <- function()
{
  if (rmarkdown::pandoc_available()) {
    
    data.frame(
      pandoc_directory = rmarkdown::pandoc_exec(),
      pandoc_version = as.character(rmarkdown::pandoc_version()), 
      stringsAsFactors = FALSE
    )
    
  } else {
    
    print("No PANDOC installed!")
  }
}
