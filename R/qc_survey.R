#' @title Quality Control - Survey Data
#'
#' @description Quality control survey data (e.g., from surveyPoint_0.csv files) imported
#' using \code{read_otg_csv()} or \code{read_otg_csv_wrapper()}.
#'
#' @author Mike Ackerman
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams get_otg_col_specs
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#' @return a tibble with QC results

qc_survey = function(qc_df = NULL,
                     otg_type = "surveyPoint_0.csv") {

  # Starting message
  cat(paste("Starting QC on otg_type =", otg_type, "data. \n"))

  # Initiate qc_tmp
  qc_tmp = qc_tbl()

  #####
  # CHECK 1: Do the column names in qc_df match the expected column names for that otg_type?
  check_col_names(qc_df,
                  otg_type)

  #####
  # CHECK 2: Are there duplicate site names?
  cat(paste("Looking for duplicate site names... \n"))
  dup_site = qc_df$`Site Name` %>%
    table() %>%
    data.frame() %>%
    .[.$Freq > 1, 1]

  if(length(dup_site) == 0) cat("No duplicate site names found. \n")
  if(length(dup_site) > 0) {
    tmp = qc_df %>%
      dplyr::filter(`Site Name` %in% dup_site) %>%
      dplyr::select(path_nm, GlobalID, `Site Name`) %>%
      dplyr::mutate(error_message = paste0(`Site Name`, " site name is a duplicate.")) %>%
      dplyr::select(- `Site Name`)

    cat("Duplicate site names found! \n")
    qc_tmp = rbind(qc_tmp, tmp)
  }

  #####
  # CHECK 3: Are there or NAs in these columns?
  tmp = check_na(qc_df,
                 cols_to_check = c("Survey Time",
                                   "Survey Date"))
}

