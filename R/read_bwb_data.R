#' Import: read_bwb_header1_meta
#'
#' @param file path(s) to EXCEL spreadsheet
#' @param meta_pattern meta_pattern default("META")
#' @param keep_pattern keep_pattern (default: \code{column_pattern_gather_ignore})
#' @param dbg debug (default: FALSE)
#' @return data.table with imported xls(x) files
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @importFrom kwb.utils selectColumns
#' @importFrom stringr str_detect
#' @importFrom data.table rbindlist
#' @export
#'
read_bwb_header1_meta <- function(
    file, 
    meta_pattern = "META", 
    keep_pattern = column_pattern_gather_ignore(),
    dbg = FALSE
) 
{
  # Get the names of the sheets in the Excel workbook
  sheets <- readxl::excel_sheets(file)
  
  # Try to find the sheet containing meta data
  meta_sheet <- get_meta_sheet_or_stop(sheets, meta_pattern, file)
  
  # Read the metadata sheet
  all_metadata <- readxl::read_excel(file, meta_sheet)
  
  # Get the names of the sheets for which metadata are available
  described_sheets <- unique(kwb.utils::selectColumns(all_metadata, "Sheet"))
  
  # Loop through the names of the sheets for which metadata are available
  sheet_data_list <- lapply(described_sheets, function(sheet) {
    
    # Filter for the metadata given for the current sheet
    metadata <- all_metadata[all_metadata$Sheet == sheet, ]
    metadata$file_name <- file
    metadata$sheet_name <- sheet
    
    # Load the data from the current sheet
    tmp_data <- readxl::read_excel(file, sheet, guess_max = 2^20)
    
    # Safely select the original column names
    columns_orig <- kwb.utils::selectColumns(metadata, "OriginalName")
    
    # Safely select the clean column names
    columns_clean <- kwb.utils::selectColumns(metadata, "Name")
    
    # Are the columns kept as columns, i. e. excluded from gathering?
    keep <- stringr::str_detect(columns_clean, keep_pattern)
    
    # Convert the data from wide to long format
    gather_and_join_1(tmp_data, columns_clean[keep], metadata, dbg)
  })
  
  # Merge all data frames in long format
  data.table::rbindlist(l = sheet_data_list, fill = TRUE)
}

#' Import: read_bwb_header2
#'
#' @param file file path(s) to EXCEL spreadsheet
#' @param skip number of rows to skip in each sheet (default: 2)
#' @param keep_pattern (default: column_pattern_gather_ignore())
#' @param site_id_pattern (default: "^[0-9]{1,4}")
#' @param dbg debug (default: TRUE)
#' @return data.table with imported xls(x) files
#' @importFrom readxl excel_sheets
#' @importFrom stringr str_detect
#' @importFrom cellranger cell_limits
#' @importFrom cellranger cell_rows
#' @importFrom data.table rbindlist
#' @export
#' 
read_bwb_header2 <- function(
    file, 
    skip = 2, 
    keep_pattern = column_pattern_gather_ignore(),
    site_id_pattern = "^[0-9]{1,4}", 
    dbg = TRUE
) 
{
  # Define helper functions, 2^20 = max number of rows in xlsx
  read_from_excel <- function(...) {
    readxl::read_excel(..., col_names = FALSE, guess_max = 2^20)
  }
  
  sheets <- readxl::excel_sheets(file)
  
  has_site_id <- stringr::str_detect(sheets, site_id_pattern)
  
  stop_on_missing_or_inform_on_extra_sheets(has_site_id, file, sheets)
  
  data_frames <- lapply(which(has_site_id), function(sheet_index) {
    
    sheet <- sheets[sheet_index]
    
    cat(sprintf(
      "FROM: %s\nReading sheet (%d/%d): %s\n",
      file, sheet_index, length(sheets), sheet
    ))
    
    # Read the header rows
    stopifnot(skip == 2L) # Otherwise we need more column names!
    
    header <- read_from_excel(
      file, 
      sheet,
      range = cellranger::cell_rows(c(1L, skip)),
    )
    
    header <- to_full_metadata_2(header, file, sheet)
    
    # Read the data rows
    tmp_content <- read_from_excel(
      file, 
      sheet,
      range = cellranger::cell_limits(
        ul = c(skip + 1L, 1L),
        lr = c(NA, nrow(header))
      )
    )
    
    indices <- match(names(tmp_content), header$col_id)
    
    names(tmp_content) <- header$key[indices]
    
    # Check content format
    tbl_datatypes <- table(unlist(sapply(tmp_content, class)))
    tbl_datatypes <- sort(tbl_datatypes, decreasing = TRUE)
    
    columns_keep <- grep(keep_pattern, names(tmp_content), value = TRUE)
    
    print_datatype_info_if(dbg, tbl_datatypes, columns_keep)
    
    # TODO: check for duplicates in names
    gather_and_join_2(tmp_content, columns_keep, header)
  })
  
  data.table::rbindlist(l = data_frames, fill = TRUE)
}

#' Import: read_bwb_header4
#'
#' @param file file path(s) to EXCEL spreadsheet
#' @param skip number of rows to skip in each sheet (default: 4)
#' @param keep_pattern (default: column_pattern_gather_ignore())
#' @param site_id_pattern (default: "^[0-9]{1,4}")
#' @param dbg debug (default: TRUE)
#' @return data.table with imported xls(x) files
#' @importFrom readxl excel_sheets
#' @importFrom stringr str_detect
#' @importFrom cellranger cell_limits
#' @importFrom cellranger cell_rows
#' @importFrom data.table rbindlist
#' @export
#' 
read_bwb_header4 <- function(
    file, 
    skip = 4, 
    keep_pattern = column_pattern_gather_ignore(),
    site_id_pattern = "^[0-9]{1,4}", 
    dbg = TRUE
) 
{
  # Define helper functions
  read_from_excel <- function(...) {
    readxl::read_xlsx(..., col_names = FALSE, guess_max = 2^20)
  }
  
  sheets <- readxl::excel_sheets(file)
  
  has_site_id <- stringr::str_detect(sheets, site_id_pattern)
  
  stop_on_missing_or_inform_on_extra_sheets(has_site_id, file, sheets)
  
  data_frames <- lapply(which(has_site_id), function(sheet_index) {
    
    sheet <- sheets[sheet_index]
    
    cat(sprintf(
      "FROM: %s\nReading sheet (%d/%d): %s\n",
      file, sheet_index, length(sheets), sheet
    ))
    
    # Read the header rows
    stopifnot(skip == 4L) # Otherwise we need more column names!
    
    header <- read_from_excel(
      file, 
      sheet,
      range = cellranger::cell_rows(c(1L, skip))
    )
    
    header <- to_full_metadata_4(header, file, sheet)
    
    # Read the data rows
    tmp_content <- read_from_excel(
      file, 
      sheet,
      range = cellranger::cell_limits(
        ul = c(skip + 1L, 1L),
        lr = c(NA, nrow(header))
      )
    )
    
    indices <- match(names(tmp_content), header$col_id)
    
    names(tmp_content) <- header$key[indices]
    
    # Check content format
    tbl_datatypes <- table(unlist(sapply(tmp_content, class)))
    tbl_datatypes <- sort(tbl_datatypes, decreasing = TRUE)
    
    columns_keep <- grep(keep_pattern, names(tmp_content), value = TRUE)
    
    print_datatype_info_if(dbg, tbl_datatypes, columns_keep)
    
    # TODO: check for duplicates in names
    gather_and_join_2(tmp_content, columns_keep, header)
  })
  
  data.table::rbindlist(l = data_frames, fill = TRUE)
}

#' Helper function: to_full_metadata2
#'
#' @param header header
#' @param file file
#' @param sheet sheet
#' @return data frame with metadata for header2 (EXCEL) files
#'
to_full_metadata_2 <- function(header, file, sheet)
{
  # Start a metadata table with the Variable name and unit
  header <- as.data.frame(t(header), stringsAsFactors = FALSE)
  
  names(header) <- c("VariableName_org", "UnitName_org")
  
  # Extend the metadata
  header$key <- kwb.utils::pasteColumns(header, sep = "@")
  header$key <- stringr::str_replace(header$key, "@NA$", "")
  header$col_id <- rownames(header)
  header$file_name <- normalizePath(file)
  header$sheet_name <- sheet
  header$site_id <- get_site_id(sheet)
  
  header
}

#' Helper function: to_full_metadata_4
#'
#' @param header header
#' @param file file
#' @param sheet sheet
#' @return data frame with metadata for header4 (EXCEL) files
#'
to_full_metadata_4 <- function(header, file, sheet)
{
  # Start a metadata table with the Variable name and unit
  header <- as.data.frame(t(header), stringsAsFactors = FALSE)
  
  names(header) <- c(
    "VariableName_org", "Method", "UnitName_org", "Limit_TrinkWV"
  )
  
  # Extend the metadata
  header$key <- kwb.utils::pasteColumns(header, sep = "@")
  header$key <- stringr::str_replace(header$key, "@NA@NA@NA$", "")
  header$col_id <- rownames(header)
  header$file_name <- normalizePath(file)
  header$sheet_name <- sheet
  header$site_id <- get_site_id(sheet)
  
  header
}

#' Helper function: gather_and_join_1
#'
#' @param tmp_data tmp_data
#' @param columns_keep columns_keep
#' @param metadata metadata
#' @param dbg dbg (default: FALSE)
#' @importFrom kwb.utils printIf
#' @importFrom tidyr gather_
#' @importFrom dplyr left_join
#' @return gathered and joined data frame

gather_and_join_1 <- function(tmp_data, columns_keep, metadata, dbg = FALSE)
{
  kwb.utils::printIf(dbg, names(tmp_data))
  kwb.utils::printIf(dbg, columns_keep)
  
  tmp_data %>%
    tidyr::gather_(
      key_col = "VariableName_org", 
      value_col = "DataValue",
      gather_cols = setdiff(names(tmp_data), columns_keep)
    ) %>%
    dplyr::left_join(
      y = metadata, 
      by = c(VariableName_org = "Name")
    )
}

#' Helper function: gather_and_join_2
#'
#' @param tmp_content tmp_content
#' @param columns_keep columns_keep
#' @param header header
#' @importFrom tidyr gather_
#' @importFrom dplyr left_join
#' @return gathered and joined data frame
gather_and_join_2 <- function(tmp_content, columns_keep, header)
{
  tmp_content %>% 
    tidyr::gather_(
      key_col = "key", 
      value_col = "DataValue",
      gather_cols = setdiff(names(tmp_content), columns_keep)
    ) %>%
    dplyr::left_join(
      y = header, 
      by = "key"
    )
}

#' Import: read_bwb_data
#' @description wrapper around \code{read_bwb_header2} and
#' \code{read_bwb_header1_meta}
#' @param files file path(s) to EXCEL spreadsheet
#' @param meta_pattern (default: "META")
#' @param keep_pattern  (default: \code{column_pattern_gather_ignore})
#' @param site_id_pattern (default: "^[0-9]{1,4}")
#' @param dbg debug (default: TRUE)
#' @return data.table with imported xls(x) files
#' @importFrom readxl excel_sheets
#' @importFrom stringr str_detect
#' @importFrom data.table rbindlist
#' @export
#' 
read_bwb_data <- function(
    files, 
    meta_pattern = "META", 
    keep_pattern = column_pattern_gather_ignore(),
    site_id_pattern = "^[0-9]{1,4}", 
    dbg = TRUE
)
{
  result_list <- lapply(files, function(file) {
    
    sheets <- readxl::excel_sheets(file)
    
    is_meta <- stringr::str_detect(sheets, meta_pattern)
    
    has_site_id <- stringr::str_detect(sheets, site_id_pattern)
    
    if (any(is_meta)) {
      
      header <- read_bwb_header1_meta(file, meta_pattern, keep_pattern)
      
      if (exists("header") && nrow(header)) {
        header
      } # else NULL implicitly
      
    } else if (any(has_site_id)) {
      
      header <- read_bwb_header2(
        file,
        keep_pattern = keep_pattern, site_id_pattern = site_id_pattern,
        dbg = dbg
      )
      
      if (exists("header") && nrow(header)) {
        header
      } # else NULL implicitly
      
    } else {
      
      cat_red_bold_0(
        "'", basename(file), "' does not follow import schemes defined ",
        "in functions read_bwb_header1_meta() or read_bwb_header2()\n"
      )
    }
  })
  
  data.table::rbindlist(l = kwb.utils::excludeNULL(result_list), fill = TRUE)
}

#' import_labor
#'
#' @param files vector with full paths of xlsx input files
#' @param export_dir export directory
#' @param func function to be used (default: \code{read_bwb_header2})
#' @return list with length equal to number of input files
#' @importFrom stats setNames
#' @importFrom utils capture.output str
#' @export
#' 
import_labor <- function(files, export_dir, func = read_bwb_header2)
{
  try_func_on_file <- function(file) try(func(file))
  
  labor <- stats::setNames(lapply(files, try_func_on_file), basename(files))
  
  #   if(capture_output) {
  #   func_txt <- gsub("::|:::", "_", as.character(substitute(func)))
  #
  #  file_name <- sprintf("%s_structure.txt", func_txt)
  #
  #  file <- file.path(export_dir, file_name, fsep = "/")
  #
  #  utils::capture.output(utils::str(labor, nchar.max = 254, list.len = 10000), file = file)
  # }
  #
  
  labor
}
