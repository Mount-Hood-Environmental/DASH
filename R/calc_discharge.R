#' @title Calculate Discharge
#'
#' @description Calculate the discharge from each station and sum them.
#' Based on the calculations from CHaMP, described on MonitoringMethods.org, which
#' states: Sum station discharge across all stations by calculating the depth x velocity x station
#' width for all stations except first and last.  Station discharge for first and last station
#' is 0.5 x station width x depth x velocity. https://www.monitoringresources.org/Document/Method/Details/853
#'
#' This function assumes the station width represents the distance between the
#' previous station and the current one. In most instances, the station widths
#' will be equal across the entire transect.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param discharge_df input data.frame of `otg_type =` "DischargeMeasurements_6.csv"
#' containing the station width, depth, and velocity data to estimate discharge
#'
#' @import dplyr
#' @export
#' @return a tibble

calc_discharge <- function(discharge_df = NULL) {

  stopifnot(!is.null(discharge_df))

  return_df = discharge_df %>%
    dplyr::select(parent_global_id,
                  tape_distance_m,
                  station_depth_m,
                  station_velocity_m_s) %>%
    dplyr::group_by(parent_global_id) %>%
    dplyr::mutate(station = 1:n()) %>%
    dplyr::mutate(min_stat = min(station),
                  max_stat = max(station)) %>%
    # calculate station width
    dplyr::mutate(stat_width_m = if_else(station == min_stat,
                                         (lead(tape_distance_m) - tape_distance_m) / 2,
                                         if_else(station == max_stat,
                                                 (tape_distance_m - lag(tape_distance_m)) / 2,
                                                 ((tape_distance_m - lag(tape_distance_m)) / 2) + ((lead(tape_distance_m) - tape_distance_m) / 2)))) %>%
    # calculate station discharge
    dplyr::mutate(stat_disch = stat_width_m * station_depth_m * station_velocity_m_s) %>%
    # calculate channel unit discharge
    dplyr::summarise(discharge_cms = sum(stat_disch),
                     .groups = "drop")

  return(return_df)
}
