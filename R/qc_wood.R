#' @title Quality Control - Wood
#'
#' @description Quality control wood data (e.g., from Wood_2.csv files) imported
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

qc_wood = function(qc_df = NULL,
                       cols_to_check_nas = c("Large Wood Number",
                                             "Length (m)",
                                             "Diameter (m)",
                                             "Wet?",
                                             "Channel Forming?",
                                             "Ballasted?")) {

  otg_type = "Wood_2.csv"

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
  # CHECK 3: any strange values in the Wet?, Ballasted? or Channel Forming? columns?
  yes_no_qc = qc_df %>%
    dplyr::select(path_nm, GlobalID,
                  `Wet?`,
                  `Channel Forming?`,
                  `Ballasted?`) %>%
    tidyr::pivot_longer(cols = c(`Wet?`,
                                 `Channel Forming?`,
                                 `Ballasted?`)) %>%
    dplyr::filter(!is.na(value),
           !value %in% c("Yes", "No")) %>%
    dplyr::mutate(error_message = paste0("Column ", name, " has an unusal value.")) %>%
    dplyr::select(one_of(names(qc_tmp)))
  if( nrow(yes_no_qc) > 0 ) qc_tmp = rbind(qc_tmp, yes_no_qc)

  #####
  # CHECK 4: Are length and diameter possibly mixed up?
  len_diam_qc = qc_df %>%
    dplyr::filter(`Length (m)` <= `Diameter (m)`) %>%
    dplyr::mutate(error_message = "Length is less than or equal to the diameter of a piece of large wood") %>%
    dplyr::select(one_of(names(qc_tmp)))
  if( nrow(len_diam_qc) > 0 ) qc_tmp = rbind(qc_tmp, len_diam_qc)

  #####
  # CHECK 5:  Are the number, length, width and height values outside of expected values?
  cat("Checking whether large wood number, length, and diamter fall within expected values? \n")

  # set expected values
  exp_values = tibble(name = c("Large Wood Number",
                               "Length (m)",
                               "Diameter (m)"),
                      min = c(0),
                      max = c(Inf,
                              100,
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

  if( nrow(val_chk) == 0 ) cat("All wood measurement values fall within expected values.")
  if( nrow(val_chk) > 0 ) {
    cat("Wood values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, val_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_jam()

