#' @title Quality Control - Wrapper Function
#'
#' @description A wrapper function to perform quality control for all on-the-ground (OTG)
#' data "types" collected using the DASH protocol.
#'
#' @author Mike Ackerman
#'
#' @param survey_df data.frame containing the survey data
#' @param cu_df data.frame containing the channel uni data
#' @param wood_df data.frame containing the wood data
#' @param jam_df data.frame containing the jam data
#' @param discharge_df data.frame containing the discharge location data
#' @param disch_meas_df data.frame containing the discharge measurement data
#' @param redirect_output would you like to redirect the output messages
#' from `qc_wrapper()` to a file instead of the R terminal? Default = `TRUE`.
#' If set to `FALSE`, messages will simply be written to console.
#' @param redirect_output_path a path and file name (e.g., .txt or .csv) to
#' write the `qc_wrapper` output messages to. Default will write
#' "qc_wrapper_output.txt" to the current working directory.
#'
#' @import dplyr
#' @importFrom tibble add_column
#' @importFrom readr write_csv
#' @export
#' @return a tibble with combined QC results

qc_wrapper = function(survey_df = NULL,
                      cu_df = NULL,
                      wood_df = NULL,
                      jam_df = NULL,
                      undercut_df = NULL,
                      discharge_df = NULL,
                      disch_meas_df = NULL,
                      redirect_output = TRUE,
                      redirect_output_path = "qc_wrapper_output.txt") {

  if( redirect_output == T ) { sink(redirect_output_path) }

  # run QC for all cases where is not NULL
  if( !is.null(survey_df) )     qc_s = qc_survey(survey_df)          else qc_s = qc_tbl()
  if( !is.null(cu_df) )         qc_c = qc_cu(cu_df)                  else qc_c = qc_tbl()
  if( !is.null(wood_df) )       qc_w = qc_wood(wood_df)              else qc_w = qc_tbl()
  if( !is.null(jam_df) )        qc_j = qc_jam(jam_df)                else qc_j = qc_tbl()
  if( !is.null(undercut_df) )   qc_u = qc_undercut(undercut_df)      else qc_u = qc_tbl()
  if( !is.null(discharge_df) )  qc_d1 = qc_disch(discharge_df)       else qc_d1 = qc_tbl()
  if( !is.null(disch_meas_df) ) qc_d2 = qc_disch_meas(disch_meas_df) else qc_d2 = qc_tbl()

  # combine results
  tmp = qc_tbl() %>%
    tibble::add_column(source = "Dummy",
                       .before = 0) %>%
    dplyr::bind_rows(qc_s %>%
                       tibble::add_column(source = "Survey",
                                          .before = 0)) %>%
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

  if( redirect_output == T ) { sink() }

  # return tmp
  return(tmp)

} # end qc_wrapper()
