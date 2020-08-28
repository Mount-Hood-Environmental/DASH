#' @title Quality Control - Discharge
#'
#' @description Quality control discharge data (e.g., from Discharge_5.csv files) imported
#' using \code{read_otg_csv()} or \code{read_otg_csv_wrapper()}.
#'
#' @author Kevin See
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams get_otg_col_specs
#' @inheritParams check_na
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
#' @return a tibble with QC results

qc_disch = function(qc_df = NULL,
                         cols_to_check_nas = c("Discharge Location (BOS, TOS, CU #)")) {

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
  na_cu = qc_df %>%
    dplyr::filter(!`Discharge Location (BOS, TOS, CU #)` %in% c('TOS', 'BOS')) %>%
    dplyr::mutate_at(vars(`Discharge Location (BOS, TOS, CU #)`),
                     list(as.numeric)) %>%
    dplyr::filter(is.na(`Discharge Location (BOS, TOS, CU #)`)) %>%
    dplyr::mutate(error_message = "Non-numeric channel unit number") %>%
    dplyr::select(one_of(names(qc_tmp)))
  if( nrow(na_cu) > 0 ) qc_tmp = rbind(qc_tmp, na_cu)

  #####
  # CHECK 4: any channel units negative?
  neg_cu = qc_df %>%
    dplyr::filter(!`Discharge Location (BOS, TOS, CU #)` %in% c('TOS', 'BOS')) %>%
    dplyr::mutate_at(vars(`Discharge Location (BOS, TOS, CU #)`),
                     list(as.numeric)) %>%
    dplyr::filter(`Discharge Location (BOS, TOS, CU #)` <= 0) %>%
    dplyr::mutate(error_message = "Non-numeric channel unit number") %>%
    dplyr::select(one_of(names(qc_tmp)))
  if( nrow(neg_cu) > 0 ) qc_tmp = rbind(qc_tmp, neg_cu)

  # return qc results
  return(qc_tmp)

} # end qc_disch()

