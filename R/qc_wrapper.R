#' @title Quality Control - Wrapper Function
#'
#' @description A wrapper function to perform quality control for all on-the-ground (otg)
#' data "types" collected using the DASH protocol.
#'
#' @author Mike Ackerman
#'
#' @param qc_df_list
#'
#' @import dplyr
#' @importFrom tibble add_column
#' @export
#' @return a tibble with combined QC results

qc_wrapper = function(survey_df = NULL,
                      cu_df = NULL,
                      wood_df = NULL,
                      jam_df = NULL,
                      undercut_df = NULL,
                      discharge_df = NULL,
                      disch_meas_df = NULL) {

  # run qc for all cases where is not NULL
  if( !is.null(survey_df) )     qc_s = qc_survey(survey_df)
  if( !is.null(cu_df) )         qc_c = qc_cu(cu_df)
  if( !is.null(wood_df) )       qc_w = qc_wood(wood_df)
  if( !is.null(jam_df) )        qc_j = qc_jam(jam_df)
  if( !is.null(undercut_df) )   qc_u = qc_undercut(undercut_df)
  if( !is.null(discharge_df) )  qc_d1 = qc_disch(discharge_df)
  if( !is.null(disch_meas_df) ) qc_d2 = qc_disch_meas(disch_meas_df)

  # combine results
  tmp = qc_s %>%
    tibble::add_column(source = "Survey",
                       .before = 0) %>%
    dplyr::bind_rows(qc_c %>%
                       tibble::add_column(source = "CU",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_w %>%
                       tibble::add_column(source = "Wood",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_j %>%
                       tibble::add_column(source = "Jam",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_u %>%
                       tibble::add_column(source = "Undercut",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_d1 %>%
                       tibble::add_column(source = "Discharge",
                                          .before = 0)) %>%
    dplyr::bind_rows(qc_d2 %>%
                       tibble::add_column(source = "DischargeMeasurements",
                                          .before = 0))

    # return tmp
    return(tmp)

} # end qc_wrapper()
