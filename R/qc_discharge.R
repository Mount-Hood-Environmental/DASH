#' @title Quality Control - Discharge
#'
#' @description Quality control discharge data (e.g., from Discharge_5.csv files) imported
#' using `read_otg_csv()` or `read_otg_csv_wrapper()`.
#'
#' @author Kevin See
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams check_na
#' @param tape_range numeric vector of minimum and maximum acceptable values for tape distance
#' @param depth_range numeric vector of minimum and maximum acceptable values for station depth
#' @param vel_range numeric vector of minimum and maximum acceptable values for station velocity
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @export
#' @return a tibble with QC results

qc_discharge = function(qc_df = NULL,
                        cols_to_check_nas = c("Tape Distance (m)",
                                              "Station Depth (m)",
                                              "Station Velocity (m/s)"),
                        tape_range = c(0, 25),
                        depth_range = c(0, 3),
                        vel_range = c(-1, 5)) {

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
  # CHECK 3: Are the tape distance, depth, and velocity values outside of expected values?
  cat("Checking whether tape distance, depth, and velocity values fall within expected values? \n")

  # set expected values
  exp_values = matrix(c(tape_range,
                        depth_range,
                        vel_range),
                      byrow = T,
                      ncol = 2,
                      dimnames = list(c("Tape Distance",
                                        "Station Depth",
                                        "Station Velocity"),
                                      c("min",
                                        "max"))) %>%
    as_tibble(rownames = "name")

  # do measured values fall outside of expected values
  val_chk = qc_df %>%
    dplyr::select(path_nm,
                  GlobalID,
                  `Tape Distance (m)`,
                  `Station Depth (m)`,
                  `Station Velocity (m/s)`) %>%
    tidyr::pivot_longer(cols = starts_with(c("Tape", "Station"))) %>%
    dplyr::left_join(exp_values) %>%
    rowwise() %>%
    dplyr::mutate(in_range = dplyr::between(value,
                                            min,
                                            max)) %>%
    dplyr::filter(!in_range) %>%
    dplyr::mutate(error_message = paste0("The measurement ", name, " (", value, ") falls outside of the expected values between ", min, " and ", max)) %>%
    dplyr::select(all_of(names(qc_tmp)))

  if( nrow(val_chk) == 0 ) cat("All discharge measurement values fall within expected values. \n")
  if( nrow(val_chk) > 0 ) {
    cat("Discharge measurement values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, val_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_discharge()
