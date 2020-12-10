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
                          cols_to_check_nas = c("CU_Number",
                                                "Site_ID",
                                                "StreamName",
                                                "CU_Type",
                                                "Seg_Number")) {

  stopifnot(!is.null(cl_sf))

  # Starting message
  cat("Starting QC on  centerline data. \n")

  # cl_sf %>%
  #   rename(GlobalID = file_row_id)

  # Initiate qc_tmp
  qc_tmp = qc_tbl(data_id)

  #####
  # CHECK 1: Are there NAs in these columns?
  tmp = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    check_na(data_id,
             cols_to_check_nas)
    # dplyr::mutate(line_num = 1:n()) %>%
    # dplyr::select(line_num, any_of(cols_to_check_nas)) %>%
    # dplyr::filter_at(., vars(cols_to_check_nas), any_vars(is.na(.))) %>%
    # dplyr::mutate(across(any_of(cols_to_check_nas),
    #                      as.character)) %>%
    # tidyr::pivot_longer(cols = any_of(cols_to_check_nas),
    #                     names_to = "col_name",
    #                     values_to = "value") %>%
    # dplyr::filter(is.na(value)) %>%
    # dplyr::mutate(error_message = paste0("Column ", col_name, " is <blank> or NA on line ", line_num, ".")) %>%
    # dplyr::select(line_num, error_message)

  if( nrow(tmp) > 0 ) qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 2: Are any channel units labeled 0?
  cu_0 = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    # dplyr::select(line_num, CU_Number) %>%
    filter(CU_Number == 0) %>%
    dplyr::mutate(error_message = "Channel unit is labeled 0.") %>%
    dplyr::select(path_nm, any_of(data_id), error_message)

  if( nrow(cu_0) > 0 ) {
    cat( nrow(cu_0), "channel units labeled 0. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_0)
  }

  #####
  # CHECK 3: Are there any duplicated channel units?
  cu_dup = cl_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(CU_Number != 0) %>%
    dplyr::group_by(Site_ID, Seg_Number, CU_Number) %>%
    dplyr::summarise(n_records = n(),
              .groups = "drop") %>%
    dplyr::filter(n_records > 1) %>%
    dplyr::left_join(cl_sf %>%
                sf::st_drop_geometry() %>%
                dplyr::as_tibble(),
                by = c("Site_ID", "Seg_Number", 'CU_Number')) %>%
    dplyr::mutate(error_message = paste0(Site_ID, ", segment ", Seg_Number, ", CU ", CU_Number, " has more than one record.")) %>%
    dplyr::select(path_nm, any_of(data_id),
                  error_message)

  if( nrow(cu_dup) > 0 ) {
    cat( nrow(cu_dup) / 2, "channel units seem to be duplicated. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, cu_dup)
  }

  # return qc results
  return(qc_tmp)

}
