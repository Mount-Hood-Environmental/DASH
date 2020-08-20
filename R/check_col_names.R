#' @title QC - Check Column Names
#'
#' @description Compare columns of a tibble or data frame to the expected
#' columns defined in \code{get_otg_col_specs}.
#'
#' @author Mike Ackerman
#'
#' @inheritParams qc_survey
#' @inheritParams get_otg_col_specs
#'
#' @export
#' @return a message

check_col_names = function(qc_df = NULL,
                           otg_type = c("surveyPoint_0.csv",
                                        "CU_1.csv",
                                        "Wood_2.csv",
                                        "Jam_3.csv",
                                        "Undercut_4.csv",
                                        "Discharge_5.csv",
                                        "DischargeMeasurements_6.csv")) {

  # check arguments
  otg_type = match.arg(otg_type)

  # starting message
  cat(paste("Comparing columns in qc_df to expected columns defined in get_otg_col_specs(). \n"))

  # expected columns
  exp_cols = get_otg_col_specs(otg_type = otg_type) %>%
    .$cols %>%
    names()

  # observed columns
  obs_cols = names(qc_df)[names(qc_df) != "path_nm"]

  # do they match?
  chk = identical(exp_cols, obs_cols)

  # the actions
  if(chk == TRUE) cat(paste("Columns match the expected! \n"))
  if(chk == FALSE) cat(paste("Column names in qc_df do not match the expected column names defined in get_otg_col_specs(). Attempting to continue...", "\n"))

} # end column_check()


