#' @title QC Channel Unit Points
#'
#' @description Quality control the channel unit points, perhaps from
#' Field Maps
#'
#' @author Mike Ackerman
#'
#' @param cu_points_sf The sf object containing the channel unit points, perhaps
#' collected in the field using Field Maps
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr unite
#' @export
#' @return a tibble with QC results

qc_cu_points = function(cu_points_sf = NULL) {

  # Starting message
  cat("Starting QC on channel unit points. \n")

  # create a blank QC tibble
  qc_tmp = tibble::tibble(
    cu_id = character(),
    error_message = character()
  )

  #####
  # CHECK 1: are there CUs that fall in multiple habitat reaches?
  cus_in_multiple_hrs = cu_pts %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::select(site_name,
                  year,
                  cu_num,
                  hab_rch) %>%
    dplyr::distinct() %>%
    tidyr::unite(cu_id,
                 site_name, year, cu_num,
                 remove = F) %>%
    dplyr::filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
    dplyr::mutate(error_message = paste0("Channel unit ", cu_id, " is within multiple habitat reaches.")) %>%
    dplyr::select(cu_id,
                  error_message)

  if( nrow(cus_in_multiple_hrs) == 0 ) cat("No CUs fall in multiple channel units. \n")
  if( nrow(cus_in_multiple_hrs) > 0 ) {
    cat(paste0(nrow(cus_in_multiple_hrs), " channel units fall in multiple channel units. Adding to QC results. \n")) %>%
      qc_tmp = rbind(qc_tmp, cus_in_multiple_hrs)
  }

  return(qc_tmp)

} # end qc_cu_points()
