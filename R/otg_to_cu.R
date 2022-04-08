#' @title Rollup Channel Unit Data
#'
#' @description Clean channel unit data (e.g., from `otg_type =` "CU_1.csv") and join
#' survey/site information to it
#'
#' @author Mike Ackerman, Kevin See, Richie Carmichael
#'
#' @inheritParams rollup_cu
#' @inheritParams rollup_cu_jam
#' @inheritParams rollup_cu_undercut
#' @inheritParams rollup_cu_wood
#' @inheritParams rollup_cu_discharge
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom janitor clean_names
#' @importFrom tidyr replace_na
#' @export
#' @return a data.frame summarizing data for channel units

otg_to_cu = function(survey_df = NULL,
                     cu_df = NULL,
                     wood_df = NULL,
                     jam_df = NULL,
                     undercut_df = NULL,
                     discharge_df = NULL,
                     fix_nas = TRUE,
                     wood_impute_cols = c('length_m',
                                          'diameter_m'),
                     jam_impute_cols  = c("length_m",
                                          "width_m",
                                          "height_m",
                                          "estimated_number_of_pieces"),
                     undercut_impute_cols = c("length_m",
                                              "width_25_percent_m",
                                              "width_50_percent_m",
                                              "width_75_percent_m"),
                     ...) {

  # if clean_names() has not already been performed on these dfs, run it now...
  survey_df %<>%
    janitor::clean_names()
  cu_df %<>%
    janitor::clean_names()
  wood_df %<>%
    janitor::clean_names()
  jam_df %<>%
    janitor::clean_names()
  undercut_df %<>%
    janitor::clean_names()
  discharge_df %<>%
    janitor::clean_names()

  # channel unit rollup
  cu_main = rollup_cu(survey_df,
                      cu_df)

  # wood rollup
  cu_wood = rollup_cu_wood(wood_df,
                           fix_nas = fix_nas,
                           impute_cols = wood_impute_cols,
                           ...)

  # wood jam rollup
  cu_jam = rollup_cu_jam(jam_df,
                         fix_nas = fix_nas,
                         impute_cols = jam_impute_cols,
                         ...)
  # undercut rollup
  cu_undct = rollup_cu_undercut(undercut_df,
                                fix_nas = fix_nas,
                                impute_cols = undercut_impute_cols,
                                ...)

  # discharge rollup
  cu_disch = rollup_cu_discharge(discharge_df,
                                 ...)

  # and combine various rollups into a single data.frame
  cu_df = cu_main %>%
    dplyr::left_join(cu_wood,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_jam,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_undct,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_disch,
                     by = c("global_id" = "parent_global_id"))

  # convert what should be true 0s from NA to 0
  cu_df = cu_df %>%
    mutate_at(vars(lwd_n:undct_area_m2),
              replace_na, 0)

  return(cu_df)

}
