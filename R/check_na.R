#' @title QC - Check For NAs
#'
#' @description Check for unexpected or important NAs within
#' specified columns.
#'
#' @author Mike Ackerman
#'
#' @param cols_to_check_nas columns to check for NAs in
#' @inheritParams qc_survey
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @export
#' @return \code{NULL} or a tibble from \code{qc_tbl()}

check_na = function(qc_df = NULL,
                    cols_to_check_nas = NULL) {

  cat("Checking for unexpected NAs in columns cols_to_check_nas: \n")
  cat(cols_to_check_nas, sep = ", ")

  # cols_to_check = c("Survey Time", "Survey Date") # for testing

  # check for NA in cols_to_check
  na_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, all_of(cols_to_check_nas)) %>%
    dplyr::filter_at(., vars(cols_to_check_nas), any_vars(is.na(.))) %>%
    tidyr::gather(key = "col_name", "value", all_of(cols_to_check_nas)) %>%
    dplyr::filter(is.na(value)) %>%
    dplyr::mutate(error_message = paste0("Column ", col_name, " is <blank> or NA.")) %>%
    dplyr::select(-col_name, -value)

  if( nrow(na_chk) == 0 ) {
    cat("No unexpected NAs found! \n")
    return(NULL)
  }

  if( nrow(na_chk) > 0 ) {
    cat("Unexpected NAs found. Adding to QC results. \n")
    return(na_chk)
  }

} # end check_na()
