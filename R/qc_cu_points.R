#' @title QC Channel Unit Points
#'
#' @description Quality control the channel unit points, perhaps from
#' Field Maps.
#'
#' @author Mike Ackerman
#'
#' @param cu_pts_sf The sf object containing the channel unit points, perhaps
#' collected in the field using Field Maps. At a minimum, the sf object should
#' contain columns containing the site/survey name, year of the survey, segment
#' number, channel unit number, and the habitat reach which defines which
#' channel units should be "lumped together" for analysis.
#' @param site_name_col The name of the column containing the site/survey name.
#' @param year_col The name of the column containing the year.
#' @param seg_num_col The name of the column containing the segment number.
#' @param cu_num_col The name of the column containing the channel unit number.
#' @param hab_reach_col The name of the column containing the habitat reach.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr unite
#' @importFrom janitor clean_names
#' @export
#' @return a tibble with QC results

qc_cu_points = function(cu_pts_sf = NULL,
                        site_name_col = "site_name",
                        year_col = "year",
                        seg_num_col = "seg_num",
                        cu_num_col = "cu_num",
                        hab_reach_col = "hab_rch") {

  stopifnot(!is.null(cu_pts_sf))

  # starting message
  cat("Starting QC on channel unit points. \n")

  #####
  # CHECK 1: are the necessary columns in cu_pts_df?
  if ( !{{site_name_col}} %in% names(cu_pts_sf) ) stop("site_name_col not found in cu_pts_sf")
  if ( !{{year_col}} %in% names(cu_pts_sf) ) stop("year_col not found in cu_pts_sf")
  if ( !{{seg_num_col}} %in% names(cu_pts_sf) ) stop("seg_num_col not found in cu_pts_sf")
  if ( !{{cu_num_col}} %in% names(cu_pts_sf) ) stop("cu_num_col not found in cu_pts_sf")
  if ( !{{hab_reach_col}} %in% names(cu_pts_sf) ) stop("hab_reach_col not found in cu_pts_sf")

  # select columns from cu_pts_sf
  cu_tmp = cu_pts_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::select({{site_name_col}},
                  {{year_col}},
                  {{seg_num_col}},
                  {{cu_num_col}},
                  {{hab_reach_col}},
                  everything()) %>%
    janitor::clean_names()

  # create a blank QC tibble
  qc_tmp = tibble::tibble(
    cu_id = character(),
    error_message = character())

  #####
  # CHECK 2: are there CUs that fall in multiple habitat reaches?
  cus_in_multiple_hrs = cu_tmp %>%
    dplyr::distinct() %>%
    tidyr::unite(cu_id,
                 {{site_name_col}}, {{year_col}}, {{cu_num_col}},
                 remove = F) %>%
    dplyr::filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
    dplyr::mutate(error_message = paste0("Channel unit ", cu_id, " is within multiple habitat reaches.")) %>%
    dplyr::select(cu_id,
                  error_message)

  if( nrow(cus_in_multiple_hrs) == 0 ) cat("No CUs fall in multiple channel units. \n")
  if( nrow(cus_in_multiple_hrs) > 0 ) {
    cat(paste0(nrow(cus_in_multiple_hrs), " channel units fall in multiple channel units. Adding to QC results. \n"))
    qc_tmp = rbind(qc_tmp, cus_in_multiple_hrs)
  }

  return(qc_tmp)

} # end qc_cu_points()
