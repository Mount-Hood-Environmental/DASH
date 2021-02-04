
#' @title Create Empty Data Tables
#'
#' @description Create a empty tibble with the same column names and
#' column types as from a typical survey of the specified type.
#'
#' @author Kevin See
#'
#' @inheritParams get_otg_col_specs
#'
#' @import dplyr
#' @importFrom readr type_convert
#' @importFrom tibble add_column
#' @return an empty tibble

create_empty_tbl = function(otg_type = c("surveyPoint_0.csv",
                                         "CU_1.csv",
                                         "Wood_2.csv",
                                         "Jam_3.csv",
                                         "Undercut_4.csv",
                                         "Discharge_5.csv",
                                         "DischargeMeasurements_6.csv")) {

  otg_type = match.arg(otg_type)

  otg_col_specs = DASH::get_otg_col_specs(otg_type)
  empty_df = matrix(NA,
                    nrow = 1,
                    ncol = length(otg_col_specs$cols),
                    dimnames = list(NA,
                                    names(otg_col_specs$cols))) %>%
    as_tibble() %>%
    mutate(across(everything(),
                  as.character)) %>%
    readr::type_convert(col_types = otg_col_specs) %>%
    tibble::add_column(path_nm = NA_character_,
                       .before = 0)
  return(empty_df)
}
