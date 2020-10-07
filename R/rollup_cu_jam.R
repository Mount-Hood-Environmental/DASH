#' @title Rollup Channel Unit Jam Data
#'
#' @description Summarize individual jam data (e.g., from `otg_type =` "Jam_3.csv") at
#' the channel unit scale
#'
#' @author Mike Ackerman and Richie Carmichael
#'
#' @param jam_df data.frame of `otg_type =` "Jam_3.csv containing the individual
#' jam data to be summarized (rolled up) to the channel unit scale
#' @param fix_ind_jam_nas if any of the length, width, height, or estimated number of pieces measurements
#' for an individual jam is missing i.e.,`NA`, would you like to fill them in? Default is `TRUE`, in which
#' case the `NA` values will be replaced using the median of that metric from the channel unit.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export
#' @return a data.frame summarizing jam at the channel unit scale

rollup_cu_jam = function(jam_df = NULL,
                         fix_ind_jam_nas = TRUE) {

  stopifnot(!is.null(jam_df))

  # store initial column names
  cols_jam_df = names(jam_df)

  # jam measurement columns
  jam_cols = c("length_m",
               "width_m",
               "height_m",
               "estimated_number_of_pieces")

  # fix missing values in individual jams
  if( fix_ind_jam_nas == TRUE ) {

    # na_cus = jam_df %>%
    #   select(parent_global_id,
    #          one_of(jam_cols)) %>%
    #   filter_at(vars(one_of(jam_cols)), any_vars(is.na(.))) %>%
    #   select(parent_global_id) %>%
    #   pull()

    fix_df = jam_df %>%
      dplyr::select(parent_global_id,
                    global_id,
                    one_of(jam_cols)) %>%
      dplyr::filter(parent_global_id %in% na_cus) %>%
      tidyr::pivot_longer(cols = one_of(jam_cols)) %>%
      dplyr::group_by(parent_global_id,
                      name) %>%
      dplyr::mutate(median = median(value, na.rm = T)) %>%
      dplyr::mutate(value = if_else(is.na(value),
                                    median,
                                    value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-parent_global_id,
                    -median) %>%
      tidyr::pivot_wider(names_from = "name",
                         values_from = "value")

    jam_df = jam_df %>%
      dplyr::select(-one_of(jam_cols)) %>%
      dplyr::left_join(fix_df) %>%
      dplyr::select(one_of(cols_jam_df))

  } # end if( fix_length_diam_nas = TRUE )

  return_df = jam_df %>%
    dplyr::select(-(creation_date:editor)) %>%
    dplyr::mutate(area_m2 = length_m * width_m,
                  vol_m3 = length_m * width_m * height_m) %>%
    dplyr::group_by(parent_global_id) %>%
    dplyr::summarise(jam_length_m = sum(length_m),
                     jam_width_m = sum(width_m),
                     jam_height_m = sum(height_m),
                     jam_area_m2 = sum(area_m2),
                     jam_vol_m3 = sum(vol_m3),
                     jam_est_n_pieces = sum(estimated_number_of_pieces))

  return(return_df)

} # end rollup_cu_jam()

