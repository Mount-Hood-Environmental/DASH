#' @title Calculate Discharge
#'
#' @description Calculate the discharge from each station and sum them.
#' Based on the calculations from CHaMP, described on MonitoringMethods.org, which
#' states: Sum station discharge across all  stations by calculating the depth x velocity x station
#' width for all stations except first and last.  Station discharge for first and last station
#' is 0.5 x station width x depth x velocity. https://www.monitoringresources.org/Document/Method/Details/853
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param discharge_meas_df input data.frame of `otg_type =` "DischargeMeasurements_6.csv"
#' containing the station width, depth, and velocity data to estimate discharge
#'
#' @import dplyr
#' @export
#' @return a tibble

calc_discharge <- function(discharge_meas_df) {

  stopifnot(!is.null(discharge_meas_df))

  disch_df = discharge_meas_df %>%
    select(path_nm,
           ParentGlobalID,
           GlobalID,
           width = `Station Width`,
           depth = `Station Depth`,
           vel = `Station Velocity`) %>%
    group_by(path_nm, ParentGlobalID) %>%
    # determine which is the first and last station at a site
    mutate(station = 1:n()) %>%
    mutate(min_stat = min(station),
           max_stat = max(station)) %>%
    # compute station width
    mutate(stat_width = if_else(station == min_stat,
                                (lead(width) - width) / 2,
                                if_else(station == max_stat,
                                        (width - lag(width)) / 2,
                                        (lead(width) - lag(width)) / 2))) %>%
    # calculate discharge at each station
    mutate(stat_disch = if_else(station %in% c(min_stat, max_stat),
                                stat_width * depth * vel * 0.5,
                                stat_width * depth * vel)) %>%
    # sum discharge across all stations at a site
    summarise(discharge = sum(stat_disch),
              .groups = "drop")

  return(disch_df)
}
