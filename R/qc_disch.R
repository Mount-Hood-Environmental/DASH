#' @title Quality Control - Discharge
#'
#' @description Quality control discharge data (e.g., from Discharge_5.csv files) imported
#' using `read_otg_csv()` or `read_otg_csv_wrapper()`.
#'
#' @author Kevin See
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams check_na
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
#' @return a tibble with QC results

qc_disch = function(qc_df = NULL,
                    cols_to_check_nas = c("GlobalID",
                                          "Tape Distance (m)",
                                          "Station Depth (m)",
                                          "Station Velocity (m/s)",
                                          "ParentGlobalID")) {

  # set otg_type
  otg_type = "Discharge_5.csv"

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
  if( !is.null(tmp) ) qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 3: any channel units not numeric?
  # na_cu = qc_df %>%
  #   dplyr::filter(!`Discharge Location (BOS, TOS, CU #)` %in% c('TOS', 'BOS')) %>%
  #   dplyr::mutate_at(vars(`Discharge Location (BOS, TOS, CU #)`),
  #                    list(as.numeric)) %>%
  #   dplyr::filter(is.na(`Discharge Location (BOS, TOS, CU #)`)) %>%
  #   dplyr::mutate(error_message = "Non-numeric channel unit number") %>%
  #   dplyr::select(all_of(names(qc_tmp)))
  # if( nrow(na_cu) > 0 ) qc_tmp = rbind(qc_tmp, na_cu)

  #####
  # CHECK 4: any channel units negative?
  # neg_cu = qc_df %>%
  #   dplyr::filter(!`Discharge Location (BOS, TOS, CU #)` %in% c('TOS', 'BOS')) %>%
  #   dplyr::mutate_at(vars(`Discharge Location (BOS, TOS, CU #)`),
  #                    list(as.numeric)) %>%
  #   dplyr::filter(`Discharge Location (BOS, TOS, CU #)` <= 0) %>%
  #   dplyr::mutate(error_message = "Non-numeric channel unit number") %>%
  #   dplyr::select(all_of(names(qc_tmp)))
  # if( nrow(neg_cu) > 0 ) qc_tmp = rbind(qc_tmp, neg_cu)

  #####
  # CHECK 5: Is there more than one ParentGlobalID for channel units within a survey?
  # We expect only 1.

  # cat("Checking for multiple ParentGlobalIDs. \n")
  #
  # id_chk = qc_df %>%
  #   dplyr::select(path_nm, ParentGlobalID) %>%
  #   dplyr::group_by(path_nm) %>%
  #   dplyr::summarise(count = n_distinct(ParentGlobalID)) %>%
  #   dplyr::filter(count > 1) %>%
  #   dplyr::mutate(GlobalID = "multiple rows",
  #                 error_message = paste0("File ", path_nm, " contains multiple ParentGlobalIDs; expect only one.")) %>%
  #   dplyr::select(-count)
  #
  # if( nrow(id_chk) == 0 ) cat("ParentGlobalIDs appear good! \n")
  # if( nrow(id_chk) > 0 ) {
  #   cat("Some files contain multiple ParentGlobalIDs when expecting only one. Adding to QC results. \n")
  #   qc_tmp = rbind(qc_tmp, id_chk)
  # }

  # return qc results
  return(qc_tmp)

} # end qc_disch()

