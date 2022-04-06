#' @title Quality Control - Centerlines
#'
#' @description Quality control centerline shapefiles.
#'
#' @author Kevin See
#'
#' @param cl_sf The centerline `sf` object
#' @inheritParams check_na
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @export
#' @return a tibble with QC results

qc_centerline <- function(cl_sf = NULL,
                          data_id = "object_id",
                          cols_to_check_nas = c("site_name",
                                                "year",
                                                "seg_num",
                                                "cu_num",
                                                "cu_type",
                                                "hab_rch")) {

  stopifnot(!is.null(cl_sf))

  # Starting message
  cat("Starting QC on  centerline data. \n")

  # Initiate qc_tmp
  qc_tmp = qc_tbl(data_id)

  #####
  # CHECK 1: Are there NAs in these columns?
  tmp = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    check_na(cols_to_check_nas = cols_to_check_nas,
             data_id = data_id)

  if(!is.null(tmp)) qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 2: Are any channel units labeled 0?
  cu_0 = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    filter(cu_num == 0) %>%
    dplyr::mutate(error_message = "Channel unit is labeled 0.") %>%
    dplyr::select(path_nm,
                  any_of(data_id),
                  error_message)

  if( nrow(cu_0) > 0 ) {
    cat( nrow(cu_0), "channel units labeled 0. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_0)
  }

  #####
  # CHECK 3: Are there any duplicated channel units?
  cu_dup = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(cu_num != 0) %>%
    dplyr::group_by(site_name, year, seg_num, cu_num) %>%
    dplyr::summarise(n_records = n(),
                     .groups = "drop") %>%
    dplyr::filter(n_records > 1) %>%
    dplyr::left_join(cl_sf %>%
                       sf::st_drop_geometry() %>%
                       dplyr::as_tibble(),
                     by = c("site_name", "year", "seg_num", "cu_num")) %>%
    dplyr::mutate(error_message = paste0(site_name, " ", year, ", segment ", seg_num, ", CU ", cu_num, " has more than one record.")) %>%
    dplyr::select(path_nm,
                  any_of(data_id),
                  error_message)

  if( nrow(cu_dup) > 0 ) {
    cat( nrow(cu_dup) / 2, "channel units seem to be duplicated. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_dup)
  }

  # return qc results
  return(qc_tmp)

}
