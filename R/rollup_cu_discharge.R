#' @title Rollup Channel Unit Discharge Data
#'
#' @description Summarize discharge data (e.g., from `otg_type =` "Discharge_5.csv") for
#' channel units where discharge was measured
#'
#' @author Mike Ackerman, Kevin See, and Richie Carmichael
#'
#' @inheritParams calc_discharge
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing discharge measurements at the channel unit scale

rollup_cu_discharge = function(discharge_df = NULL) {

  stopifnot(!is.null(discharge_df))

  return_df = calc_discharge(discharge_df = discharge_df) %>%
    mutate(discharge_cfs = discharge_cms * 35.314666212661) %>%
    mutate(discharge_cms = round(discharge_cms, 2)) %>%
    mutate(discharge_cfs = round(discharge_cfs, 2))

  return(return_df)

} # end rollup_cu_discharge()
