#' @title Quality Control - Channel Unit Rollup
#'
#' @description Quality control the channel unit rollup
#' performed by `otg_to_cu()`. This is primarily concerned with
#' making sure that all pieces of data are connected through the
#' parent global IDs.
#'
#' @author Kevin See
#'
#' @inheritParams otg_to_cu
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom tibble add_column
#' @importFrom janitor clean_names
#' @export
#' @return a tibble with QC results

qc_rollup = function(survey_df = NULL,
                     cu_df = NULL,
                     jam_df = NULL,
                     undercut_df = NULL,
                     wood_df = NULL,
                     discharge_df = NULL,
                     discharge_meas_df = NULL) {

  # you need at least these 2 files to run the function
  stopifnot(!is.null(survey_df),
            !is.null(cu_df))

  # # if discharge_meas_df exists, then so should discharge_df, and vice versa
  # if(!is.null(discharge_meas_df) & is.null(discharge_df)) {
  #   stop("Discharge measurements exist, but no discharge_df.")
  # }
  # if(is.null(discharge_meas_df) & !is.null(discharge_df)) {
  #   stop("If discharge_df exists, then so should discharge_meas_df.")
  # }

  # fix any NULL values in other data.frames
  if(is.null(wood_df)) wood_df = create_empty_tbl("Wood_2.csv")
  if(is.null(jam_df)) jam_df = create_empty_tbl("Jam_3.csv")
  if(is.null(undercut_df)) undercut_df = create_empty_tbl("Undercut_4.csv")
  # if(is.null(discharge_df)) discharge_df = create_empty_tbl("Discharge_5.csv")
  # if(is.null(discharge_meas_df)) discharge_meas_df = create_empty_tbl("DischargeMeasurements_6.csv")

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
  discharge_df  %<>%
    clean_names()
  # if(!is.null(discharge_df)) {
  #   discharge_df %<>%
  #     clean_names()
  #   discharge_meas_df %<>%
  #     clean_names()
  # }

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
  miss_rollup = wood_df %>%
    select(path_nm,
           global_id,
           parent_global_id) %>%
    distinct() %>%
    anti_join(cu_df %>%
                select(parent_global_id = global_id),
              by = c('parent_global_id')) %>%
    tibble::add_column(source = "Wood",
                       .before = 0) %>%
    bind_rows(jam_df %>%
                select(path_nm,
                       global_id,
                       parent_global_id) %>%
                distinct() %>%
                anti_join(cu_df %>%
                            select(parent_global_id = global_id),
                          by = c('parent_global_id')) %>%
                tibble::add_column(source = "Jam",
                                   .before = 0)) %>%
    bind_rows(undercut_df %>%
                select(path_nm,
                       global_id,
                       parent_global_id) %>%
                distinct() %>%
                anti_join(cu_df %>%
                            select(parent_global_id = global_id),
                          by = c('parent_global_id')) %>%
                tibble::add_column(source = "Undercut",
                                   .before = 0)) %>%
    filter(!is.na(parent_global_id))

  if( nrow(miss_rollup) > 0 ) {
    miss_parent = miss_rollup %>%
      rename(GlobalID = global_id,
             ParentGlobalID = parent_global_id) %>%
      mutate(error_message = "There exists child data for this source, but no channel unit global ID to match this parent global ID to.") %>%
      select(any_of(names(qc_tmp)))

    qc_tmp = rbind(qc_tmp,
                   miss_parent)
  }

  # if(!is.null(discharge_df) & !is.null(discharge_meas_df)) {

    #---------------------------------------------------------
    # these discharge measurements don't have a discharge global ID to be assigned to
    # miss_disch_meas = discharge_meas_df  %>%
    #   select(path_nm,
    #          parent_global_id) %>%
    #   distinct() %>%
    #   anti_join(discharge_df %>%
    #               select(parent_global_id = global_id),
    #             by = c('parent_global_id')) %>%
    #   tibble::add_column(source = "DischargeMeas",
    #                      .before = 0) #%>%
    # left_join(calc_discharge(discharge_meas_df = discharge_meas_df),
    #           by = c('parent_global_id')) %>%
    # left_join(discharge_meas_df %>%
    #             group_by(path_nm,
    #                    parent_global_id) %>%
    #             summarise(n_meas = n_distinct(global_id),
    #                       .groups = "drop"),
    #           by = c('parent_global_id'))


    # if( nrow(miss_disch_meas) > 0 ) {
    #   qc_miss_disch_meas = miss_disch_meas %>%
    #     tibble::add_column(GlobalID = "Multiple") %>%
    #     rename(ParentGlobalID = parent_global_id) %>%
    #     mutate(error_message = "Discharge measurements were taken, but the parent global ID
    #          does not match any global ID in the Discharge_5.csv file.") %>%
    #     select(any_of(names(qc_tmp)))
    #
    #   qc_tmp = rbind(qc_tmp,
    #                  qc_miss_disch_meas)
    # }


    # these discharge IDs don't have a survey to match up with
  #   miss_disch_surv = discharge_df %>%
  #     rename(channel_unit_number = discharge_location_bos_tos_cu_number) %>%
  #     mutate(across(channel_unit_number,
  #                   str_pad,
  #                   width = 3,
  #                   pad = "0")) %>%
  #     select(path_nm,
  #            parent_global_id,
  #            global_id,
  #            # discharge_location_bos_tos_cu_number) %>%
  #            channel_unit_number) %>%
  #     distinct() %>%
  #     anti_join(survey_df %>%
  #                 select(parent_global_id = global_id),
  #               by = c('parent_global_id')) %>%
  #     tibble::add_column(source = "Discharge",
  #                        .before = 0)
  #
  #   if( nrow(miss_disch_surv) > 0 ) {
  #     qc_miss_disch_surv = miss_disch_surv %>%
  #       rename(GlobalID = global_id,
  #              ParentGlobalID = parent_global_id) %>%
  #       mutate(error_message = paste("Discharge data was collected here, at channel unit", channel_unit_number,
  #                                    "but no survey global ID exists to match to this parent global ID to.")) %>%
  #       select(any_of(names(qc_tmp)))
  #
  #     qc_tmp = rbind(qc_tmp,
  #                    qc_miss_disch_surv)
  #   }
  #
  #   # these discharge IDs don't have a channel unit to match up with
  #   miss_disch_cu = discharge_df %>%
  #     rename(channel_unit_number = discharge_location_bos_tos_cu_number) %>%
  #     mutate(across(channel_unit_number,
  #                   str_pad,
  #                   width = 3,
  #                   pad = "0")) %>%
  #     select(path_nm,
  #            parent_global_id,
  #            global_id,
  #            # discharge_location_bos_tos_cu_number) %>%
  #            channel_unit_number) %>%
  #     distinct() %>%
  #     anti_join(cu_df %>%
  #                 mutate(across(channel_unit_number,
  #                               str_pad,
  #                               width = 3,
  #                               pad = "0")) %>%
  #                 select(parent_global_id,
  #                        channel_unit_number),
  #               by = c('parent_global_id',
  #                      "channel_unit_number")) %>%
  #     tibble::add_column(source = "Discharge",
  #                        .before = 0) %>%
  #     left_join(survey_df %>%
  #                 select(parent_global_id = global_id,
  #                        site_name,
  #                        survey_date))
  #
  #   if( nrow(miss_disch_cu) > 0 ) {
  #     qc_miss_disch_cu = miss_disch_cu %>%
  #       rename(GlobalID = global_id,
  #              ParentGlobalID = parent_global_id) %>%
  #       mutate(error_message = paste("A discharge measurement from site", site_name, "at channel unit", channel_unit_number,
  #                                    "was taken, but no channel unit parent global ID and channel unit number match.")) %>%
  #       select(any_of(names(qc_tmp)))
  #
  #     qc_tmp = rbind(qc_tmp,
  #                    qc_miss_disch_cu)
  #   }
  #
  #   # bind the various missing discharge errors together
  #   miss_discharge = miss_disch_meas %>%
  #     bind_rows(miss_disch_surv) %>%
  #     bind_rows(miss_disch_cu)
  #
  # } else {
  #   miss_discharge = NULL
  # }

  return(list(error_df = qc_tmp,
              miss_rollup = miss_rollup))
}
