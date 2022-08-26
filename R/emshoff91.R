#' Create EMSHOFF91 Import Data Frame
#'
#' @param ods_dir directory to ".ods" files created manually by importing original
#' ".wq1" files into LibreOffice 7.0 on Ubuntu with encoding (Western Europe
#' (DOS/OS2-437/US)) and exporting to ".ods" format
#' @param files_to_ignore tidied names of files to ignore due to complex data input
#' structure not yet covered by importer (default: c("cl25", "gf_gm", "clliste",
#' "gwnguete", "rupelauf", "salzlast"))
#' @return data frame with columns "ods_paths" (full paths to ".ods" files),
#' "ods_files" (their "basenames") and "ods_names_clean" (tidied names used as
#' identifier)
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_remove
#' @importFrom dplyr case_when mutate
#' @examples 
#' \dontrun{
#' ods_dir <- "<replace-with-path-to-files>/emshoff91/converted_ods"
#' emshoff91_import <- create_emshoff91_import(ods_dir)
#' }
create_emshoff91_import <- function(ods_dir,
                                    files_to_ignore = c("cl25",
                                                        "clliste",
                                                        "gf_gm",
                                                        "gwnguete",
                                                        "rupelauf",
                                                        "salzlast")) {
  emshoff91 <- tibble::tibble(
    ods_paths = list.files(
      path = ods_dir,
      pattern = "\\.ods$",
      full.names = TRUE
    ),
    ods_files = basename(ods_paths),
    ods_names_clean = ods_files %>%
      stringr::str_remove(pattern = "\\.ods$") %>%
      janitor::make_clean_names()
  ) %>%
    dplyr::mutate(
      skip_rows = dplyr::case_when(
        stringr::str_detect(.data$ods_files, "^B[0-9]") ~ 1,
        stringr::str_detect(.data$ods_files, "^EWGES") ~ 1,
        stringr::str_detect(.data$ods_files, "^EWMITTEL") ~ 1,
        stringr::str_detect(.data$ods_files, "^GWBR") ~ 1,
        stringr::str_detect(.data$ods_files, "^JURA") ~ 1,
        stringr::str_detect(.data$ods_files, "^K[0-9]") ~ 1,
        stringr::str_detect(.data$ods_files, "^KREIDE") ~ 1,
        stringr::str_detect(.data$ods_files, "^PRAERUP") ~ 1,
        stringr::str_detect(.data$ods_files, "^UF") ~ 1,
        stringr::str_detect(.data$ods_files, "^GWNMITTE") ~ 2,
        stringr::str_detect(.data$ods_files, "^GWNHOCH") ~ 2,
        stringr::str_detect(.data$ods_files, "^GWNTIEF") ~ 2,
        stringr::str_detect(.data$ods_files, "^Q13") ~ 2,
        TRUE ~ 0
      )
    )
  
  
  emshoff91 %>%
    dplyr::filter(!.data$ods_names_clean %in% files_to_ignore)
}




#' Reads a Single EMSHOFF 91 ODS File into Tibble
#'
#' @param emshoff91_import_selected a row as retrieved by \link{\code{create_emshoff91_import}}
#'
#' @return imports ods file into tibble
#' @export
#'
#' @importFrom readODS read_ods
#' @importFrom janitor clean_names
#' @importFrom dplyr everything filter if_any bind_cols
#' @importFrom kwb.utils catAndRun
read_emshoff91_ods <- function(emshoff91_import_selected) {
  tmp <-
    readODS::read_ods(path = emshoff91_import_selected$ods_paths,
                      skip = emshoff91_import_selected$skip_rows) %>%
    janitor::clean_names() %>%
    #dplyr::rename("bemerkungen" = .data$x) %>%
    ### remove completely empty rows
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) 
    
  
  has_messstellen <- stringr::str_detect(names(tmp), "^messstellen$")
  
  if (any(has_messstellen)) {
    names(tmp)[has_messstellen] <- "messstelle"
  }
  
  ### hack to remove duplicated header (in row 299 in "BM_H2.ods") 
  has_datum <- stringr::str_detect(names(tmp), "^datum$")
  
  if (any(has_datum)) {
    tmp <- tmp %>% 
      dplyr::filter(.data$datum != "Datum")
  }
  
    tmp <- tmp %>%
    dplyr::filter(!is.na(.data$messstelle))
  
  if (emshoff91_import_selected$skip_rows %in% 1:2) {
    messprogramm <-
      paste0(
        readODS::read_ods(
          path = emshoff91_import_selected$ods_paths[1],
          range = sprintf("A1:B%s", emshoff91_import_selected$skip_rows),
          col_names = FALSE
        )[seq_len(emshoff91_import_selected$skip_rows), 1],
        collapse = "@"
      )
  } else {
    messprogramm <- NA_character_
  }
  
  dplyr::bind_cols(tmp, tibble::tibble("messprogramm" = messprogramm))
}


#' Reads Multiple EMSHOFF 91 ODS Files into List
#'
#' @param emshoff91_import a tibble as retrieved by \link{\code{create_emshoff91_import}}
#'
#' @return imports mulitple ods files into a list of tibbles
#' @export
#' @importFrom stats setNames
#' @importFrom kwb.utils catAndRun
#' @examples
#' \dontrun{
#' ods_dir <- "<replace-with-path-to-files>/emshoff91/emshoff91/converted_ods"
#' ods_dir <- "C:/users/mrustl/Downloads/emshoff91/emshoff91/converted_ods"
#' emshoff91_import <- create_emshoff91_import(ods_dir)
#' read_multiple_emshoff91_ods(emshoff91_import)
#' }
read_multiple_emshoff91_ods <- function(emshoff91_import) {
  emshoff91_import_list <-
    stats::setNames(lapply(seq_len(nrow(emshoff91_import)),
                           function(i) {
                             kwb.utils::catAndRun(sprintf("Importing '%s'\n",
                                                          emshoff91_import$ods_paths[i]),
                                                  expr = {
                                                    read_emshoff91_ods(emshoff91_import[i,])
                                                  })
                           }),
                    nm = emshoff91_import$ods_names_clean)
}




# tmp <- readODS::read_ods(path = emshoff91$ods_paths[13],
#                          skip = 202)
#
# tmp_list <- lapply(seq_len(nrow(tmp)), function(i) {
#        print(i)
#        stringr::str_split_fixed(tmp[i,], pattern = "\\s+", n = 29)
# })
#
# emshoff91_list <- stats::setNames(
#   lapply(seq_len(nrow(emshoff91)), function(i) {
#     dat <- emshoff91[i, ]
#     kwb.utils::catAndRun(sprintf("Importing '%s'", dat$ods_paths),
#                          expr = {
#                            readODS::read_ods(path = dat$ods_paths)
#                          })
#
#   }),
#   nm = emshoff91$ods_names_clean)
