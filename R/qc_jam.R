#' @title Quality Control - Jams
#'
#' @description Quality control jam data (e.g., from Jam_3.csv files) imported
#' using \code{read_otg_csv()} or \code{read_otg_csv_wrapper()}.
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

qc_jam = function(qc_df = NULL,
                  cols_to_check_nas = c("Length (m)",
                                        "Width (m)",
                                        "Height (m)",
                                        "Estimated Number of Pieces")) {

  # set otg_type
  otg_type = "Jam_3.csv"

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
  # CHECK 3:  Are the number, length, width and height values outside of expected values?
  cat("Checking whether number of pieces, length, width and height fall within expected values? \n")

  # set expected values
  exp_values = tibble(name = c("Estimated Number of Pieces",
                               "Length (m)",
                               "Width (m)",
                               "Height (m)"),
                      min = c(0),
                      max = c(1e3,
                              100,
                              10,
                              5))

  # do measured values fall outside of expected values
  val_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID,
                  one_of(exp_values$name)) %>%
    tidyr::pivot_longer(cols = -c(path_nm, GlobalID)) %>%
    dplyr::left_join(exp_values) %>%
    rowwise() %>%
    # TRUE = good, FALSE = outside expected values
    dplyr::mutate(in_range = dplyr::between(value,
                                            min,
                                            max)) %>%
    dplyr::filter(!in_range) %>%
    dplyr::mutate(error_message = paste0("The measurement ", name, " falls outside of the expected values between ", min, " and ", max)) %>%
    dplyr::select(one_of(names(qc_tmp)))

  if( nrow(val_chk) == 0 ) cat("All jam measurement values fall within expected values.")
  if( nrow(val_chk) > 0 ) {
    cat("Jam measurement values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, val_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_jam()

