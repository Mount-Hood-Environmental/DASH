#' @title Rollup Channel Unit Discharge Data
#'
#' @description Summarize discharge data (e.g., from `otg_type =` "Discharge_5.csv" and
#' "DischargeMeasurements_6.csv") for channel units where discharge was measured
#'
#' @author Mike Ackerman, Kevin See, and Richie Carmichael
#'
#' @param discharge_df data.frame of `otg_type =` "Discharge_5.csv" containing the survey
#' and channel unit number information where discharges were measured
#' @inheritParams calc_discharge
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing discharge measurements at the channel unit scale

rollup_cu_discharge = function(discharge_df = NULL,
                               discharge_meas_df = NULL) {

  stopifnot(!is.null(discharge_df))
  stopifnot(!is.null(discharge_meas_df))

  disch_tmp = calc_discharge(discharge_meas_df = discharge_meas_df)

  return_df = discharge_df %>%
    select(global_id,
           parent_global_id,
           discharge_location_bos_tos_cu_number) %>%
    left_join(disch_tmp,
              by = c("global_id" = "parent_global_id"))

  return(return_df)

} # end rollup_cu_discharge()
