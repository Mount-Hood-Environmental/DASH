#' @title Quality Control - Undercuts
#'
#' @description Quality control undercut data (e.g., from Undercut_4.csv files) imported
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

qc_undercut = function(qc_df = NULL,
                       cols_to_check_nas = c("GlobalID",
                                             "Location",
                                             "Length (m)",
                                             "Width 25% (m)",
                                             "Width 50% (m)",
                                             "Width 75% (m)",
                                             "ParentGlobalID")) {

  # set otg_type
  otg_type = "Undercut_4.csv"

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
  # CHECK 3:  Do all the locations have acceptable values?
  loc_qc = qc_df %>%
    filter(!Location %in% c("Right_Bank",
                            "Left_Bank",
                            "Island"),
           !is.na(Location)) %>%
    mutate(error_message = paste0("Strange value, ", Location, ", in Location column")) %>%
    select(all_of(names(qc_tmp)))
  if( !is.null(loc_qc) ) qc_tmp = rbind(qc_tmp, loc_qc)

  #####
  # CHECK 4:  Are the number, length and width values outside of expected values?
  cat("Checking whether undercut length and width fall within expected values? \n")

  # set expected values
  exp_values = tibble(name = c("Length (m)",
                               "Width 25% (m)",
                               "Width 50% (m)",
                               "Width 75% (m)"),
                      min = c(0),
                      max = c(10,
                              1.5,
                              1.5,
                              1.5))


  # do measured values fall outside of expected values
  val_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID,
                  c("Length (m)",
                    "Width 25% (m)",
                    "Width 50% (m)",
                    "Width 75% (m)")) %>%
    tidyr::pivot_longer(cols = -c(path_nm, GlobalID)) %>%
    dplyr::left_join(exp_values) %>%
    rowwise() %>%
    # TRUE = good, FALSE = outside expected values
    dplyr::mutate(in_range = dplyr::between(value,
                                            min,
                                            max)) %>%
    dplyr::filter(!in_range) %>%
    dplyr::mutate(error_message = paste0("The ", name, " measurement ", value, " is outside of the expected values of ", min, " and ", max)) %>%
    dplyr::select(all_of(names(qc_tmp)))

  if( nrow(val_chk) == 0 ) cat("All undercut measurement values fall within expected values. \n")
  if( nrow(val_chk) > 0 ) {
    cat("Undercut measurement values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, val_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_undercut()

