#' @title Quality Control - Wrapper Function
#'
#' @description A wrapper function to perform quality control for all on-the-ground (OTG)
#' data "types" collected using the DASH protocol.
#'
#' @author Mike Ackerman
#'
#' @param survey_df data.frame containing the survey data (required to run `qc_wrapper()`)
#' @param cu_df data.frame containing the channel unit data (required to run `qc_wrapper()`)
#' @param wood_df data.frame containing the wood data
#' @param jam_df data.frame containing the jam data
#' @param discharge_df data.frame containing the discharge location data
#' @param discharge_meas_df data.frame containing the discharge measurement data
#' @param channel_unit_roll_qc should there be a quality control check on the roll-up
#' to the channel unit scale, using the `qc_rollup()` function? Default is `FALSE`.
#' @param redirect_output would you like to redirect the output messages
#' from `qc_wrapper()` to a file instead of the R terminal? Default = `FALSE`.
#' @param redirect_output_path a path and file name (e.g., .txt or .csv) to
#' write the `qc_wrapper` output messages to if `redirect_output = TRUE`.
#' @param ... various other inputs to `qc_*` functions, if need to change from default values
#'
#' @import dplyr
#' @importFrom tibble add_column
#' @importFrom readr write_csv
#' @importFrom tidyr unite
#' @export
#' @return a tibble with combined QC results

qc_wrapper = function(survey_df = NULL,
                      cu_df = NULL,
                      wood_df = NULL,
                      jam_df = NULL,
                      undercut_df = NULL,
                      discharge_df = NULL,
                      #discharge_meas_df = NULL,
                      channel_unit_roll_qc = FALSE,
                      redirect_output = FALSE,
                      redirect_output_path = "qc_wrapper_output.txt",
                      ...) {

  if( redirect_output == T ) { sink(redirect_output_path) }

  # you need at least these 2 files to run the function
  stopifnot(!is.null(survey_df),
            !is.null(cu_df))

  # run QC for all cases where is not NULL
  if( !is.null(survey_df) )         qc_s = qc_survey(survey_df,...)              else qc_s = qc_tbl()
  if( !is.null(cu_df) )             qc_c = qc_cu(cu_df,...)                      else qc_c = qc_tbl()
  if( !is.null(wood_df) )           qc_w = qc_wood(wood_df,...)                  else qc_w = qc_tbl()
  if( !is.null(jam_df) )            qc_j = qc_jam(jam_df,...)                    else qc_j = qc_tbl()
  if( !is.null(undercut_df) )       qc_u = qc_undercut(undercut_df,...)          else qc_u = qc_tbl()
  if( !is.null(discharge_df) )      qc_d1 = qc_disch(discharge_df,...)           else qc_d1 = qc_tbl()
  #if( !is.null(discharge_meas_df) ) qc_d2 = qc_disch_meas(discharge_meas_df,...) else qc_d2 = qc_tbl()

  # combine results
  tmp = qc_tbl() %>%
    tibble::add_column(source = "Dummy",
                       .before = 0) %>%
    tibble::add_column(location_id = character(),
                       .after = Inf) %>%
    dplyr::bind_rows(qc_s %>%
                       left_join(survey_df %>%
                                   select(GlobalID,
                                          location_id = `Site Name`)) %>%
                       tibble::add_column(source = "Survey",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_c %>%
                       left_join(cu_df %>%
                                   left_join(survey_df %>%
                                               select(GlobalID,
                                                      `Site Name`),
                                             by = c("ParentGlobalID" = "GlobalID")) %>%
                                   dplyr::mutate(dplyr::across(c(`Channel Segment Number`,
                                                                 `Channel Unit Number`),
                                                               str_pad,
                                                               width = 3,
                                                               pad = "0")) %>%
                                   tidyr::unite("location_id",
                                                `Site Name`,
                                                `Channel Segment Number`,
                                                `Channel Unit Number`) %>%
                                   select(GlobalID,
                                          location_id)) %>%
                       tibble::add_column(source = "CU",
                                          .before = 0))

  if( !is.null(wood_df) ) {
    tmp = tmp %>%
      dplyr::bind_rows(qc_w %>%
                         left_join(wood_df %>%
                                     select(GlobalID,
                                            ParentGlobalID)) %>%
                         left_join(cu_df %>%
                                     left_join(survey_df %>%
                                                 select(GlobalID,
                                                        `Site Name`),
                                               by = c("ParentGlobalID" = "GlobalID")) %>%
                                     dplyr::mutate(dplyr::across(c(`Channel Segment Number`,
                                                                   `Channel Unit Number`),
                                                                 str_pad,
                                                                 width = 3,
                                                                 pad = "0")) %>%
                                     tidyr::unite("location_id",
                                                  `Site Name`,
                                                  `Channel Segment Number`,
                                                  `Channel Unit Number`) %>%
                                     select(GlobalID,
                                            location_id),
                                   by = c("ParentGlobalID" = "GlobalID")) %>%
                         dplyr::select(-ParentGlobalID) %>%
                         tibble::add_column(source = "Wood",
                                            .before = 0))
  } # end if wood_df NOT NULL

  if( !is.null(jam_df) ) {
    tmp = tmp %>%
      dplyr::bind_rows(qc_j %>%
                         left_join(jam_df %>%
                                     select(GlobalID,
                                            ParentGlobalID)) %>%
                         left_join(cu_df %>%
                                     left_join(survey_df %>%
                                                 select(GlobalID,
                                                        `Site Name`),
                                               by = c("ParentGlobalID" = "GlobalID")) %>%
                                     dplyr::mutate(dplyr::across(c(`Channel Segment Number`,
                                                                   `Channel Unit Number`),
                                                                 str_pad,
                                                                 width = 3,
                                                                 pad = "0")) %>%
                                     tidyr::unite("location_id",
                                                  `Site Name`,
                                                  `Channel Segment Number`,
                                                  `Channel Unit Number`) %>%
                                     select(GlobalID,
                                            location_id),
                                   by = c("ParentGlobalID" = "GlobalID")) %>%
                         dplyr::select(-ParentGlobalID) %>%
                         tibble::add_column(source = "Jam",
                                            .before = 0))
  } # end if jam_df NOT NULL

  if( !is.null(undercut_df) ) {
    tmp = tmp %>%
      dplyr::bind_rows(qc_u %>%
                         left_join(undercut_df %>%
                                     select(GlobalID,
                                            ParentGlobalID)) %>%
                         left_join(cu_df %>%
                                     left_join(survey_df %>%
                                                 select(GlobalID,
                                                        `Site Name`),
                                               by = c("ParentGlobalID" = "GlobalID")) %>%
                                     dplyr::mutate(dplyr::across(c(`Channel Segment Number`,
                                                                   `Channel Unit Number`),
                                                                 str_pad,
                                                                 width = 3,
                                                                 pad = "0")) %>%
                                     tidyr::unite("location_id",
                                                  `Site Name`,
                                                  `Channel Segment Number`,
                                                  `Channel Unit Number`) %>%
                                     select(GlobalID,
                                            location_id),
                                   by = c("ParentGlobalID" = "GlobalID")) %>%
                         dplyr::select(-ParentGlobalID) %>%
                         tibble::add_column(source = "Undercut",
                                            .before = 0))
  } # end if undercut_df NOT NULL

  if( !is.null(discharge_df) ) {
    tmp = tmp %>%
      dplyr::bind_rows(qc_d1 %>%
                         left_join(discharge_df %>%
                                     select(GlobalID,
                                            ParentGlobalID,
                                            loc = `Discharge Location (BOS, TOS, CU #)`)) %>%
                         left_join(survey_df %>%
                                     select(GlobalID,
                                            `Site Name`),
                                   by = c("ParentGlobalID" = "GlobalID")) %>%
                         tidyr::unite("location_id",
                                      `Site Name`,
                                      loc) %>%
                         dplyr::select(-ParentGlobalID) %>%
                         tibble::add_column(source = "Discharge",
                                            .before = 0))
  } # end if discharge_df NOT NULL

  # if( !is.null(discharge_meas_df) ) {
  #   tmp = tmp %>%
  #     dplyr::bind_rows(qc_d2 %>%
  #                        left_join(discharge_meas_df %>%
  #                                    select(GlobalID,
  #                                           disch_id = ParentGlobalID)) %>%
  #                        left_join(discharge_df %>%
  #                                    select(disch_id = GlobalID,
  #                                           surv_id = ParentGlobalID,
  #                                           loc = `Discharge Location (BOS, TOS, CU #)`)) %>%
  #                        left_join(survey_df %>%
  #                                    select(surv_id = GlobalID,
  #                                           `Site Name`)) %>%
  #                        tidyr::unite("location_id",
  #                                     `Site Name`,
  #                                     loc) %>%
  #                        dplyr::select(-disch_id, -surv_id) %>%
  #                        tibble::add_column(source = "DischargeMeasurements",
  #                                           .before = 0))
  # } # end if discharge_meas_df NOT NULL

  if(channel_unit_roll_qc) {
    # perform some QC on entire channel unit data
    qc_roll = qc_rollup(survey_df = survey_df,
                        cu_df = cu_df,
                        jam_df = jam_df,
                        undercut_df = undercut_df,
                        wood_df = wood_df,
                        discharge_df = discharge_df)
                        #discharge_meas_df = discharge_meas_df)

    if(nrow(qc_roll$error_df) > 0) {
      tmp = tmp %>%
        bind_rows(qc_roll$error_df)
    }
  }

  # put columns in specific order
  tmp %<>%
    select(source:GlobalID,
           any_of(c("ParentGlobalID",
                    "location_id")),
           error_message)

  if( redirect_output == T ) { sink() }

  # return tmp
  return(tmp)

} # end qc_wrapper()
