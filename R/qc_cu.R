#' @title Quality Control - Channel Unit Data
#'
#' @description Quality control channel unit data (e.g., from CU_1.csv files) imported
#' using `read_otg_csv()` or `read_otg_csv_wrapper()`.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams check_na
#' @param valid_cus character vector of valid channel unit types
#' @param ted_min minimum acceptable value for thalweg exit depth
#' @param ted_max maximum acceptable value for thalweg exit depth
#' @param md_min minimum acceptable value for max channel unit depth
#' @param md_max maximum acceptable value for max channel unit depth
#' @param cover_columns character vector of column names for fish cover estimates
#' @param cov_max maximum value that cover estimates can sum to
#' @param ocular_columns character vector of column names for ocular substrate estimates
#' @param peb_min minimum acceptable size for pebble size (mm)
#' @param peb_max maximum acceptable size for pebble size (mm)
#'
#' @import dplyr stringr
#' @export
#' @return a tibble with QC results

qc_cu = function(qc_df = NULL,
                 cols_to_check_nas = c("Channel Segment Number",
                                       "Channel Unit Number",
                                       "Channel Unit Type",
                                       "Maximum Depth (m)",
                                       "TOS",
                                       "BOS"),
                 valid_cus = c("Pool", "Run", "Riffle", "OCA", "Rapid+", "SSC"),
                 ted_min = 0,
                 ted_max = 2,
                 md_min = 0,
                 md_max = 3,
                 cover_columns = c("Overhanging (%)",
                                   "Aquatic Vegetation (%)",
                                   "Woody Debris (%)",
                                   "Artificial (%)",
                                   "Total No Cover (%)"),
                 cov_max = 130,
                 ocular_columns = c("Sand/Fines <2mm (%)",
                                    "Gravel 2-64mm (%)",
                                    "Cobble 64-256mm (%)",
                                    "Boulder >256mm (%)"),
                 peb_min = 0.03,
                 peb_max = 1024) {

  # set otg_type
  otg_type = "CU_1.csv"

  # Starting message
  cat(paste("Starting QC on otg_type =", otg_type, "data. \n"))

  # Initiate qc_tmp
  qc_tmp = qc_tbl()

  #####
  # CHECK 1: Do the column names in qc_df match the expected column names for that otg_type?
  check_col_names(qc_df = qc_df,
                  otg_type = otg_type)

  #####
  # CHECK 2: Are there NAs in these columns?
  tmp = check_na(qc_df,
                 cols_to_check_nas)
  qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 3: Are the channel unit types all valid?
  cat("Checking whether channel unit types are valid. \n")

  cu_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`) %>%
    dplyr::filter(!`Channel Unit Type` %in% valid_cus) %>%
    dplyr::mutate(error_message = paste0(`Channel Unit Type`, " is an invalid channel unit type.")) %>%
    dplyr::select(-`Channel Unit Type`)

  if( nrow(cu_chk) == 0 ) cat("All channel unit types are valid! \n")
  if( nrow(cu_chk) > 0 ) {
    cat("Some channel unit types are invalid. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_chk)
  }

  #####
  # CHECK 4: Are all channel unit numbers within each survey unique?
  cat("Are all channel unit numbers within each survey unique? \n")

  cu_num = qc_df %>%
    dplyr::select(ParentGlobalID, `Channel Unit Number`) %>%
    dplyr::group_by(ParentGlobalID) %>%
    dplyr::count(`Channel Unit Number`) %>%
    dplyr::filter(n != 1) %>%
    dplyr::mutate(GlobalID = "multiple rows",
                  error_message = paste0("Channel unit number ", `Channel Unit Number`, " appears more than once.")) %>%
    dplyr::select(-`Channel Unit Number`, -n)

  if( nrow(cu_num) == 0) cat("Yes, all channel unit numbers appear to be unique. \n")
  if( nrow(cu_num) > 0 ) {
    cat("Some channel units appear within a survey more than once. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_num)
  }

  #####
  # CHECK 5: Are maximum and thalweg exit depths filled in? The exception are OCAs, which do not require a thalweg exit depth.
  dpth_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`, `Maximum Depth (m)`, `Thalweg Exit Depth (m)`) %>%
    tidyr::pivot_longer(cols = ends_with("(m)"),
                        names_to = "measurement",
                        values_to = "value") %>%
    dplyr::filter(!(measurement == "Thalweg Exit Depth (m)" & `Channel Unit Type` == "OCA")) %>%
    dplyr::mutate(is_na = is.na(value)) %>%
    dplyr::filter(is_na == TRUE) %>%
    dplyr::mutate(error_message = paste0("The ", measurement, "value is missing.")) %>%
    dplyr::select(-c(`Channel Unit Type`,
                     measurement,
                     value,
                     is_na))

  if( nrow(dpth_chk) == 0 ) cat("Appropriate depths are filled in! \n")
  if( nrow(dpth_chk) > 0 ) {
    cat("Depth values are missing for ", nrow(dpth_chk), " channel units. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, dpth_chk)
  }

  #####
  # CHECK 6: Are maximum depth values within an expected range?
  cat("Do maximum depth values fall within an expected range btw", md_min, "and", md_max, "? \n")

  md_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Maximum Depth (m)`) %>%
    dplyr::mutate(md_chk = between(`Maximum Depth (m)`,
                                   md_min,
                                   md_max)) %>%
    dplyr::filter(md_chk == FALSE) %>%
    dplyr::mutate(error_message = paste0("Maximum depth of ", `Maximum Depth (m)`,
                                         " falls outside of expected values or is NA.")) %>%
    dplyr::select(-`Maximum Depth (m)`,
                  -md_chk)

  if( nrow(md_chk) == 0 ) cat("All maximum depth values seem reasonable. \n")
  if( nrow(md_chk) > 0 ) {
    cat("Some maximum depth values either fall outside expected values or are NA. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, md_chk)
  }

  #####
  # CHECK 7: Are thalweg exit depth values within an expected range?
  cat("Do thalweg exit depth values fall within an expected range btw", ted_min, "and", ted_max, "? \n")

  ted_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID,
                  `Channel Unit Type`,
                  `Thalweg Exit Depth (m)`) %>%
    dplyr::filter(!`Channel Unit Type` == "OCA") %>%
    dplyr::select(-`Channel Unit Type`) %>%
    # TRUE = good, FALSE = outside expected values or NA
    dplyr::mutate(ted_chk = between(`Thalweg Exit Depth (m)`,
                                    ted_min,
                                    ted_max)) %>%
    dplyr::filter(ted_chk == FALSE) %>%
    dplyr::mutate(error_message = paste0("Thalweg exit depth of ", `Thalweg Exit Depth (m)`,
                                         " falls outside of expected values or is NA.")) %>%
    dplyr::select(-`Thalweg Exit Depth (m)`,
                  -ted_chk)

  if( nrow(ted_chk) == 0 ) cat("All thalweg exit depths seem reasonable. \n")
  if( nrow(ted_chk) > 0 ) {
    cat("Some thalweg exit depth values either fall outside expected values or are NA. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, ted_chk)
  }

  #####
  # CHECK 8: Do cover column values sum to btw 100 and cov_max?
  cat("Do all fish cover columns sum btw 100 and", cov_max, "? \n")

  cov_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, all_of(cover_columns)) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(cover_sum = rowSums(.[3:(2+length(cover_columns))])) %>%
    dplyr::select(-all_of(cover_columns)) %>%
    dplyr::filter(!between(cover_sum, 100, cov_max)) %>%
    dplyr::mutate(error_message = paste0("Cover values sum to ", cover_sum, ", not between 100 and ", cov_max, ".")) %>%
    dplyr::select(-cover_sum)

  if( nrow(cov_chk) == 0 ) cat(paste0("Fish cover values for all channel units are between 100 and ", cov_max, "!\n"))
  if( nrow(cov_chk) > 0 ) {
    cat(paste0("The cover values for ", nrow(cov_chk), " channel units do not fall between 100 and ", cov_max, ". Adding to QC results. \n"))
    qc_tmp = rbind(qc_tmp, cov_chk)
  }

  #####
  # CHECK 9: Do all channel units have ocular estimates and do they sum to 100?
  cat("Do ocular estimates for all channel units exist and sum to 100? \n")

  oc_cus = valid_cus
  oc_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`, all_of(ocular_columns)) %>%
    dplyr::filter(`Channel Unit Type` %in% oc_cus) %>%
    dplyr::select(-`Channel Unit Type`) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(ocular_sum = round(rowSums(.[3:(2+length(ocular_columns))]))) %>%
    dplyr::select(-all_of(ocular_columns)) %>%
    dplyr::filter(!ocular_sum == 100) %>%
    dplyr::mutate(error_message = paste0("Ocular estimates sum to ", ocular_sum, ", not 100.")) %>%
    dplyr::select(-ocular_sum)

  if( nrow(oc_chk) == 0 ) cat("Ocular estimates for all channel units sum to 100! \n")
  if( nrow(oc_chk) > 0 ) {
    cat("The ocular estimates for", nrow(oc_chk), "channel units do not sum to 100. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, oc_chk)
  }

  #####
  # CHECK 10: Do riffles with pebble counts have all columns filled?
  cat("Do riffles with pebble counts have all values filled and within expected values? \n")

  peb_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`, starts_with("Pebble")) %>%
    dplyr::filter(`Channel Unit Type` == "Riffle") %>%
    dplyr::select(-`Channel Unit Type`) %>%
    dplyr::mutate(na_count = rowSums(is.na(.))) %>%
    dplyr::filter(!na_count == 11) %>%
    dplyr::filter(!na_count == 0) %>%
    dplyr::select(-starts_with("Pebble")) %>%
    dplyr::mutate(error_message = paste0("Pebble size values for ", na_count, " columns within the pebble counts appear to be NA.")) %>%
    dplyr::select(-na_count)

  if( nrow(peb_chk) == 0 ) cat("Size values appear to be filled in for riffles where pebble counts occurred. \n")
  if( nrow(peb_chk) > 0 ) {
    cat("Some pebble size values for", nrow(peb_chk), "riffles appear to be missing where pebble counts occurred. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, peb_chk)
  }

  #####
  # CHECK 11: Do pebble values fall within an expected range?
  cat("Do the pebble size values fall within a reasonable range btw", peb_min, "and", peb_max, "? \n")

  peb_sz_chk = qc_df %>%
    dplyr::filter(`Channel Unit Type` == "Riffle") %>%
    dplyr::select(path_nm, GlobalID, starts_with("Pebble")) %>%
    tidyr::pivot_longer(cols = starts_with("Pebble"),
                        names_to = "measurement",
                        values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(peb_chk = between(value,
                                   peb_min,
                                   peb_max)) %>%
    dplyr::filter(peb_chk == FALSE) %>%
    dplyr::mutate(error_message = paste0("Pebble size of ", stringr::str_remove(measurement, " \\(mm\\)$"),
                                         " is ", value,
                                         " which does not fall between ", peb_min, " and ", peb_max, ".")) %>%
    dplyr::select(-measurement,
                  -value,
                  -peb_chk)

  if( nrow(peb_sz_chk) == 0 ) cat("All pebble size values seem reasonable. \n")
  if( nrow(peb_sz_chk) > 0 ) {
    cat("Some pebble size values fall outside expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, peb_sz_chk)
  }

  #####
  # CHECK 12: Is there more than one ParentGlobalID for channel units within a survey?
  # We expect only 1.
  cat("Checking for multiple ParentGlobalIDs. \n")

  id_chk = qc_df %>%
    dplyr::select(path_nm, ParentGlobalID) %>%
    dplyr::group_by(path_nm) %>%
    dplyr::summarise(count = n_distinct(ParentGlobalID)) %>%
    dplyr::filter(count > 1) %>%
    dplyr::mutate(GlobalID = "multiple rows",
                  error_message = paste0("File ", path_nm, " contains multiple ParentGlobalIDs; expect only one.")) %>%
    dplyr::select(-count)

  if( nrow(id_chk) == 0 ) cat("ParentGlobalIDs appear good! \n")
  if( nrow(id_chk) > 0 ) {
    cat("Some files contain multiple ParentGlobalIDs when expecting only one. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, id_chk)
  }

  #####
  # CHECK 13: Are widths filled in for all SSCs?
  cat("Make sure widths are filled in for all SSCs. \n")

  ssc_chk = qc_df %>%
    dplyr::filter(`Channel Unit Type` == "SSC") %>%
    dplyr::select(path_nm, GlobalID, starts_with("Width")) %>%
    dplyr::mutate(na_count = rowSums(is.na(.))) %>%
    dplyr::filter(na_count > 0) %>%
    dplyr::mutate(error_message = paste0("Missing width for SSC.")) %>%
    dplyr::select(-na_count,
                  -starts_with("Width"))

  if( nrow(ssc_chk) == 0 ) cat("Widths for SSCs are filled in! \n")
  if( nrow(ssc_chk) > 0 ) {
    cat("Widths are missing for ", nrow(ssc_chk), " SSCs. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, ssc_chk)

  }

  #####
  # CHECK 14: Are there any non-SSCs with widths filled in?
  cat("Are there any non-SSC with widths filled in, which should not be the case? \n")
  nonssc_chk = qc_df %>%
    dplyr::filter(`Channel Unit Type` != "SSC") %>%
    dplyr::select(path_nm, GlobalID, starts_with("Width")) %>%
    dplyr::mutate(value_count = rowSums(!is.na(.))) %>%
    dplyr::filter(value_count > 2) %>%
    dplyr::mutate(error_message = "A width is filled in for a non-SSC.") %>%
    dplyr::select(-value_count,
                  -starts_with("Width"))

  if( nrow(nonssc_chk) == 0 ) cat("Widths for non-SSCs are empty, which is good! \n")
  if( nrow(nonssc_chk) > 0 ) {
    cat("Some widths are filled in for ", nrow(nonssc_chk), " non-SSCs. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, nonssc_chk)
  }

  #####
  # CHECK 15: Are there any non-SSCs with widths filled in?
  cat("Does each survey have a TOS and BOS defines? \n")
  tos_bos_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, ParentGlobalID, TOS, BOS) %>%
    dplyr::filter(TOS == T | BOS == T) %>%
    tidyr::pivot_longer(cols = c(BOS, TOS),
                        names_to = "location",
                        values_to = "value") %>%
    dplyr::filter(value == T) %>%
    dplyr::group_by(ParentGlobalID) %>%
    dplyr::mutate(n_true = dplyr::n()) %>%
    dplyr::filter(n_true != 2) %>%
    dplyr::mutate(error_message = paste0("Survey ", ParentGlobalID, " within ", path_nm, "appears to have incorrect TOS & BOS.")) %>%
    dplyr::select(-GlobalID,
                  -location,
                  -value,
                  -n_true) %>%
    dplyr::rename(GlobalID = ParentGlobalID)

  if( nrow(tos_bos_chk) == 0 ) cat("Each survey appears to have a TOS & BOS! \n")
  if( nrow(tos_bos_chk) > 0 ) {
    cat(nrow(bos_tos_chk), "surveys appear to have errant TOS or BOS. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, bos_tos_chk)
  }

  ###################
  # return qc results
  return(qc_tmp)

} # end qc_cu()
