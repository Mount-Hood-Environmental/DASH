#' @title Quality Control - Channel Unit Data
#'
#' @description Quality control channel unit data (e.g., from CU_1.csv files) imported
#' using \code{read_otg_csv()} or \code{read_otg_csv_wrapper()}.
#'
#' @author Mike Ackerman
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams check_na
#'
#' @import dplyr
#' @export
#' @return a tibble with QC results

qc_cu = function(qc_df = NULL,
                 cols_to_check_nas = c("Channel Unit Type",
                                       "Channel Unit Number",
                                       "Channel Segment Number",
                                       "Maximum Depth (m)",
                                       "ParentGlobalID")) {

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
  #if( nrow(tmp) > 0 ) qc_tmp = rbind(qc_tmp, tmp)
  qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 3: Are the channel unit types all valid?
  cat("Checking whether all channel unit types are valid. \n")

  valid_cus = c("Pool", "Run", "Riffle", "OCA", "Rapid+", "SSC")
  cu_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`) %>%
    #rbind("bad_cu") %>% # for testing
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
    dplyr::select(path_nm, `Channel Unit Number`) %>%
    dplyr::group_by(path_nm) %>%
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
  # CHECK 5: Are the thalweg exit depth values within a reasonable range?
  ted_min = 0; ted_max = 2
  cat("Do the thalweg exit depth values fall within a reasonable range btw", ted_min, "and", ted_max, "? \n")

  ted_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`, `Thalweg Exit Depth (m)`) %>%
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
  # CHECK 6: Are the maximum depth values within a reasonable range?
  md_min = 0; md_max = 3
  cat("Do the maximum depth values fall within a reasonable range btw", md_min, "and", md_max, "? \n")

  md_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Maximum Depth (m)`) %>%
    dplyr::mutate(md_chk = between(`Maximum Depth (m)`,
                                   md_min,
                                   md_max)) %>%
    dplyr::filter(md_chk == FALSE) %>%
    dplyr::mutate(error_message = paste0("Maximum depth of", `Maximum Depth (m)`,
                                          " falls outside of expected values or is NA.")) %>%
    dplyr::select(-`Maximum Depth (m)`,
                  -md_chk)

  if( nrow(md_chk) == 0 ) cat("All maximum depth values seem reasonable. \n")
  if( nrow(md_chk) > 0 ) {
    cat("Some maximum depth values either fall outside expected values or are NA. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, md_chk)
  }

  #####
  # CHECK 7: Do cover column values sum to 100?
  cat("Do all fish cover columns sum to 100? \n")

  # What columns contain fish cover data?
  cover_columns = c("Overhanging Cover",
                    "Aquatic Vegetation",
                    "Woody Debris Cover",
                    "Artificial Cover",
                    "Total No Cover")

  # What is the maximum value that cover estimates can sum to?
  cov_max = 130

  cov_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, all_of(cover_columns)) %>%
    replace(is.na(.), 0) %>%
    #dplyr::mutate(cover_sum = round(rowSums(.[3:(2+length(cover_columns))]))) %>%
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
  # CHECK 8: Do all slow water channel unit types have ocular estimates and do they sum to 100?
  cat("Do ocular estimates for all slow water channel unit types exist and sum to 100? \n")

  # What are the slow channel unit types?
  slow_cus = c("Pool", "Run", "OCA")

  # What columns contain substrate ocular estimates?
  ocular_columns = c("Sand/Fines 2mm",
                     "Gravel 2-64mm",
                     "Cobble 64-256mm",
                     "Boulder 256mm")

  oc_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, `Channel Unit Type`, all_of(ocular_columns)) %>%
    dplyr::filter(`Channel Unit Type` %in% slow_cus) %>%
    dplyr::select(-`Channel Unit Type`) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(ocular_sum = round(rowSums(.[3:(2+length(ocular_columns))]))) %>%
    dplyr::select(-all_of(ocular_columns)) %>%
    dplyr::filter(!ocular_sum == 100) %>%
    dplyr::mutate(error_message = paste0("Ocular estimates sum to ", ocular_sum, ", not 100.")) %>%
    dplyr::select(-ocular_sum)

  if( nrow(oc_chk) == 0 ) cat("Ocular estimates for all slow channel units sum to 100! \n")
  if( nrow(oc_chk) > 0 ) {
    cat("The ocular estimates for", nrow(oc_chk), "slow channel units do not sum to 100. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, oc_chk)
  }

  #####
  # CHECK 9: Do riffles with pebble counts have all columns filled? And if so, do values
  # fall within expected values?
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
  # CHECK 10: Do we need to add a check to see if pebble values fall within an expected range?
  # expected values (mm)
  peb_min = 0.06; peb_max = 1024

  ###################
  # return qc results
  return(qc_tmp)

} # end qc_cu()
