#' @title Quality Control - Channel Unit Rollup
#'
#' @description Quality control the channel unit rollup
#' performed by `otg_to_cu()`
#'
#' @author Kevin See
#'
#' @inheritParams otg_to_cu
#'
#' @export
#' @return a tibble with QC results

qc_rollup = function(cu_df = NULL,
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


  # rollup each type of data
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

  # rollup everything at the channel unit scale
  cu_df = otg_to_cu(cu_df,
                    survey_df,
                    jam_df,
                    undercut_df,
                    wood_df,
                    discharge_df,
                    discharge_meas_df,
                    fix_nas = TRUE,
                    jam_impute_cols,
                    undercut_impute_cols,
                    wood_impute_cols,
                    ...)

  # Initiate qc_tmp
  qc_tmp = tibble(source = character(),
                  parent_global_id = character(),
                  error_message = character())

  #---------------------------------------------------------
  # any rollup values that didn't get included in cu_df?
  miss_rollup = cu_wood %>%
    anti_join(cu_main,
              by = c('parent_global_id' = 'global_id')) %>%
    mutate(source = "Wood") %>%
    select(source, parent_global_id) %>%
    bind_rows(cu_jam %>%
                anti_join(cu_main,
                          by = c('parent_global_id' = 'global_id')) %>%
                mutate(source = "Jam") %>%
                select(source, parent_global_id)) %>%
    bind_rows(cu_undct %>%
                anti_join(cu_main,
                          by = c('parent_global_id' = 'global_id')) %>%
                mutate(source = "Undercut") %>%
                select(source, parent_global_id))

  if( nrow(miss_rollup) > 0 ) {
    miss_parent = miss_rollup %>%
      mutate(error_message = "There exists child data for this source, but no channel unit global ID to match this parent global ID to.")
    qc_tmp = rbind(qc_tmp,
                   miss_parent)
  }

  #---------------------------------------------------------
  # these discharge measurements don't have a channel unit to be assigned to
  miss_discharge = cu_disch %>%
    select(-global_id) %>%
    anti_join(cu_main,
              by = c("parent_global_id",
                     "channel_unit_number")) %>%
    mutate(source = "Discharge") %>%
    select(source, everything()) %>%
    left_join(survey_df %>%
                select(parent_global_id = global_id,
                       site_name,
                       survey_date))

  if( nrow(miss_discharge) > 0 ) {
    qc_miss_discharge = miss_discharge %>%
      mutate(error_message = paste("A discharge measurement from site", site_name, "at channel unit", channel_unit_number,
                                   "was taken but no channel unit global ID exists to match this parent global ID to.")) %>%
      select(any_of(names(qc_tmp)))

    qc_tmp = rbind(qc_tmp,
                   qc_miss_discharge)
  }


  return(list(error_df = qc_tmp,
              miss_rollup = miss_rollup,
              miss_discharge = miss_discharge))
}
