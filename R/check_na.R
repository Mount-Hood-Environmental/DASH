#' @title QC - Check For NAs
#'
#' @description Check for unexpected or important NAs within
#' specified columns.
#'
#' @author Mike Ackerman
#'
#' @param cols_to_check_nas columns to check for NAs in
#' @inheritParams qc_tbl
#' @inheritParams qc_survey
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @export
#' @return `NULL` or a tibble from `qc_tbl()`

check_na = function(qc_df = NULL,
                    cols_to_check_nas = NULL,
                    data_id = "GlobalID") {

  cat("Checking for unexpected NAs in cols_to_check_nas: ")
  cat(cols_to_check_nas, sep = ", "); cat("\n")

  # check for NA in cols_to_check
  na_chk = qc_df %>%
    dplyr::select(any_of(c("path_nm", data_id)), all_of(cols_to_check_nas)) %>%
    dplyr::mutate(across(any_of(cols_to_check_nas),
                         as.character)) %>%
    tidyr::gather(key = "col_name",
                  value = "value",
                  any_of(cols_to_check_nas)) %>%
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


