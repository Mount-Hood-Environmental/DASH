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
#' @export
#' @return a data.frame summarizing data for channel units

rollup_channel_unit = function(cu_df = NULL,
                               survey_df = NULL,
                               jam_df = NULL,
                               undercut_df = NULL,
                               wood_df = NULL,
                               discharge_df = NULL,
                               discharge_meas_df = NULL,
                               fix_nas = TRUE,
                               jam_impute_cols  = c("length_m",
                                                    "width_m",
                                                    "height_m",
                                                    "estimated_number_of_pieces"),
                               undercut_impute_cols = c("length_m",
                                                        "width_25_percent_m",
                                                        "width_50_percent_m",
                                                        "width_75_percent_m"),
                               wood_impute_cols = c('length_m',
                                                    'diameter_m'),
                               ...) {

  cu_main = rollup_cu(cu_df,
                      survey_df)
  cu_jam = rollup_cu_jam(jam_df,
                         fix_nas = fix_nas,
                         impute_cols = jam_impute_cols,
                         ...)
  cu_undct = rollup_cu_undercut(undercut_df,
                                fix_nas = fix_nas,
                                impute_cols = undercut_impute_cols,
                                ...)
  cu_wood = rollup_cu_wood(wood_df,
                           fix_nas = fix_nas,
                           impute_cols = wood_impute_cols,
                           ...)
  cu_disch = rollup_cu_discharge(discharge_df,
                                 discharge_meas_df) %>%
    rename(channel_unit_number = discharge_location_bos_tos_cu_number) %>%
    mutate(across(channel_unit_number,
                  str_pad,
                  width = 3,
                  pad = "0"))

  # combine various rollups into a single data.frame
  cu_df = cu_main %>%
    dplyr::left_join(cu_wood,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_jam,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_undct,
                     by = c("global_id" = "parent_global_id")) %>%
    dplyr::left_join(cu_disch %>%
                       select(-global_id))

  return(cu_df)

}
