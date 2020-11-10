#' @title Quality Control - Discharge Measurements
#'
#' @description Quality control discharge measurement data (e.g., from DischargeMeasurements_6.csv files) imported
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

qc_disch_meas = function(qc_df = NULL,
                         cols_to_check_nas = c("Station Width",
                                               "Station Depth",
                                               "Station Velocity")) {

  # set otg_type
  otg_type = "DischargeMeasurements_6.csv"

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
  # CHECK 3:  Are the width, depth or velocity outside of expected values?
  cat("Checking whether width, depth and velocity fall within expected values? \n")

  # set expected values
  exp_values = tibble(name = c("Station Width",
                               "Station Depth",
                               "Station Velocity"),
                      min = c(0,0,-1),
                      max = c(50,5,10))


  # do measured values fall outside of expected values
  val_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID,
                  `Station Width`,
                  `Station Depth`,
                  `Station Velocity`) %>%
    tidyr::pivot_longer(cols = starts_with("Station")) %>%
    dplyr::left_join(exp_values) %>%
    rowwise() %>%
    # TRUE = good, FALSE = outside expected values
    dplyr::mutate(in_range = dplyr::between(value,
                                            min,
                                            max)) %>%
    dplyr::filter(!in_range) %>%
    dplyr::mutate(error_message = paste0("The measurement ", name, " falls outside of the expected values between ", min, " and ", max)) %>%
    dplyr::select(one_of(names(qc_tmp)))

  if( nrow(val_chk) == 0 ) cat("All discharge measurement values fall within expected values. \n")
  if( nrow(val_chk) > 0 ) {
    cat("Discharge measurement values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, val_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_disch_meas()

