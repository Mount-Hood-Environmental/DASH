#' @title Quality Control - Channel Unit Rollup
#'
#' @description Quality control the channel unit rollup
#' performed by `otg_to_cu()`
#'
#' @author Kevin See
#'
#' @inheritParams otg_to_cu
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom tibble add_column
#' @export
#' @return a tibble with QC results

qc_rollup = function(survey_df = NULL,
                     cu_df = NULL,
                     jam_df = NULL,
                     undercut_df = NULL,
                     wood_df = NULL,
                     discharge_df = NULL,
                     discharge_meas_df = NULL,
                     fix_nas = FALSE,
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

  # ensure naming conventions are what's expected
  survey_df %<>%
    clean_names()
  cu_df %<>%
    clean_names()
  jam_df %<>%
    clean_names()
  undercut_df %<>%
    clean_names()
  wood_df %<>%
    clean_names()
  discharge_df %<>%
    clean_names()
  discharge_meas_df %<>%
    clean_names()

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

  # # rollup everything at the channel unit scale
  # cu_rollup = otg_to_cu(survey_df,
  #                   cu_df,
  #                   jam_df,
  #                   undercut_df,
  #                   wood_df,
  #                   discharge_df,
  #                   discharge_meas_df,
  #                   fix_nas = fix_nas,
  #                   jam_impute_cols = jam_impute_cols,
  #                   undercut_impute_cols = undercut_impute_cols,
  #                   wood_impute_cols = wood_impute_cols,
  #                   ...)

  # Initiate qc_tmp
  qc_tmp = qc_tbl() %>%
    tibble::add_column(source = character(),
                       .before = 0) %>%
    tibble::add_column(ParentGlobalID = character(),
                       .before = "error_message")

  #---------------------------------------------------------
  # any survey data missing from cu_df?
  dangling_survey = survey_df %>%
    anti_join(cu_df,
              by = c('global_id' = 'parent_global_id')) %>%
    select(path_nm,
           global_id)

  if(nrow(dangling_survey) > 0) {
    dangling_survey %<>%
      rename(GlobalID = global_id,
             ParentGlobalID = parent_global_id) %>%
      mutate(source = "Survey") %>%
      mutate(error_message = "This survey global ID does not have any channel unit data associated with it.")

    qc_tmp = rbind(qc_tmp,
                   dangling_survey)
  }

  # any cu_df not have a survey parent global ID?
  dangling_cu = cu_df %>%
    anti_join(survey_df,
              by = c('parent_global_id' = 'global_id')) %>%
    select(path_nm,
           global_id,
           parent_global_id) %>%
    distinct()

  if(nrow(dangling_cu) > 0) {
    dangling_cu %<>%
      rename(GlobalID = global_id,
             ParentGlobalID = parent_global_id) %>%
      mutate(source = "CU") %>%
      mutate(error_message = "This channel unit parent global ID does not have a survey global ID associated with it.")

    qc_tmp = rbind(qc_tmp,
                   dangling_cu)
  }


  #---------------------------------------------------------
  # any rollup values that didn't get included in cu_df?
  miss_rollup = cu_wood %>%
    anti_join(cu_main,
              by = c('parent_global_id' = 'global_id')) %>%
    mutate(source = "Wood") %>%
    left_join(wood_df %>%
                select(parent_global_id,
                       global_id,
                       path_nm) %>%
                distinct()) %>%
    select(source, path_nm,
           global_id,
           parent_global_id) %>%
    bind_rows(cu_jam %>%
                anti_join(cu_main,
                          by = c('parent_global_id' = 'global_id')) %>%
                mutate(source = "Jam") %>%
                left_join(jam_df %>%
                            select(parent_global_id,
                                   global_id,
                                   path_nm) %>%
                            distinct()) %>%
                select(source, path_nm,
                       global_id,
                       parent_global_id)) %>%
    bind_rows(cu_undct %>%
                anti_join(cu_main,
                          by = c('parent_global_id' = 'global_id')) %>%
                mutate(source = "Undercut") %>%
                left_join(undercut_df %>%
                            select(parent_global_id,
                                   global_id,
                                   path_nm) %>%
                            distinct()) %>%
                select(source, path_nm,
                       global_id,
                       parent_global_id))

  if( nrow(miss_rollup) > 0 ) {
    miss_parent = miss_rollup %>%
      rename(GlobalID = global_id,
             ParentGlobalID = parent_global_id) %>%
      mutate(error_message = "There exists child data for this source, but no channel unit global ID to match this parent global ID to.") %>%
      select(any_of(names(qc_tmp)))

    qc_tmp = rbind(qc_tmp,
                   miss_parent)
  }

  #---------------------------------------------------------
  # these discharge measurements don't have a channel unit to be assigned to
  miss_discharge = cu_disch %>%
    anti_join(cu_main,
              by = c("parent_global_id",
                     "channel_unit_number")) %>%
    mutate(source = "Discharge") %>%
    left_join(discharge_df %>%
                select(path_nm,
                       global_id)) %>%
    select(source, everything()) %>%
    left_join(survey_df %>%
                select(parent_global_id = global_id,
                       site_name,
                       survey_date))

  if( nrow(miss_discharge) > 0 ) {
    qc_miss_discharge = miss_discharge %>%
      rename(GlobalID = global_id,
             ParentGlobalID = parent_global_id) %>%
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
