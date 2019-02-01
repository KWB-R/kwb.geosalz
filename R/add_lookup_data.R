#' get_parameters_meta
#'
#' @param xlsx_path  path to EXCEL spreadsheet with parameter metadata
#' @param sheet_name name of sheet containing metadata table 
#' (default: "nur Parameterliste")
#' @return imported parameter metadata with cleaned columns names
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' 
get_parameters_meta <- function(xlsx_path,
                                sheet_name = "nur Parameterliste") {

  readxl::read_excel(xlsx_path, sheet = sheet_name, .name_repair = "minimal") %>%
    janitor::clean_names()
}


#' add_para_metadata
#'
#' @param df df 
#' @param lookup_para_path lookup_para_path
#' @param parameters_path parameters_path
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import crayon
#' @importFrom rlang .data
#' @importFrom utils write.csv read.csv
#' @return return "df" with added parameter metadata
#' @export

add_para_metadata <- function(df,
                              lookup_para_path,
                              parameters_path) {

  if (file.exists(lookup_para_path)) {
    lookup_para <- read.csv(
      file = lookup_para_path,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(!is.na(.data$para_id))

    if (length(lookup_para) > 0) {
      parameters <- get_parameters_meta(parameters_path)

      lookup_para <- lookup_para %>%
        dplyr::left_join(y = parameters)

      analysed_paras <- unique(lookup_para$para_kurzname)
      cat(crayon::green(
        crayon::bold(
          sprintf(
            "Successfully read parameter lookup table:\n'%s'\n and joined it with:\n
                  '%s'.\n\nIn total the following %d parameters can be analysed:\n%s\n\n",
            lookup_para_path,
            parameters_path,
            length(analysed_paras),
            paste(analysed_paras, collapse = ", ")
          )
        )
      ))

      labor_sel <- df %>%
        dplyr::left_join(y = lookup_para, by = "VariableName_org") %>%
        dplyr::filter(!is.na(.data$para_id))
    } else {
      stop(sprintf("No parameters defined in %s", lookup_para_path))
    }
  } else {
    lookup_para_template <- data.frame(
      VariableName_org = sort(as.character(unique(df[, "VariableName_org"]))),
      para_id = NA,
      stringsAsFactors = FALSE
    )

    lookup_para_export_path <- file.path(
      dirname(lookup_para_path),
      "lookup_para_tmp.csv"
    )

    write.csv(lookup_para_template,
      file = lookup_para_export_path,
      row.names = FALSE
    )

    stop_msg <- cat(crayon::red(
      crayon::bold(sprintf(
        "Template parameter lookup table created at:\n%s\n
                             Please fill with column 'para_id' with 'Para Id' from file:\n%s\n
                             Do not use EXCEL, just a text editor like Notepad++.\n
                             Afterwords rename the file to 'lookup_para.csv' and save it in the same directory!",
        normalizePath(lookup_para_export_path),
        normalizePath(parameters_path)
      ))
    ))

    stop(stop_msg)
  }
  return(labor_sel)
}

#' add_site_metadata 
#'
#' @param df df 
#' @param site_path site_path 
#'
#' @return data frame with added site metadata
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
add_site_metadata <- function(df,
                              site_path) {


  sites <- readxl::read_excel(path = site_path, 
                              .name_repair = "minimal") %>%
    janitor::clean_names() %>%
    dplyr::rename(site_id = .data$interne_nr)

  df %>%
    dplyr::left_join(y = sites, by = "site_id")
}
