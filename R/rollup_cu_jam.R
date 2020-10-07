#' @title Rollup Channel Unit Jam Data
#'
#' @description Summarize individual jam data (e.g., from `otg_type =` "Jam_3.csv") at
#' the channel unit scale
#'
#' @author Mike Ackerman and Richie Carmichael
#'
#' @param jam_df data.frame of `otg_type =` "Jam_3.csv containing the individual
#' jam data to be summarized (rolled up) to the channel unit scale
#' @param fix_nas if any of the length, width, height, or estimated number of pieces measurements
#' for an individual jam is missing i.e.,`NA`, would you like to fill them in? Default is `TRUE`, in which
#' case the `NA` values will be imputed using function \code{impute_missing_values}.
#' @param ... other arguments to \code{impute_missing_values}
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing jam at the channel unit scale

rollup_cu_jam = function(jam_df = NULL,
                         fix_nas = TRUE,
                         ...) {

  stopifnot(!is.null(jam_df))

  # jam measurement columns
  jam_cols = c("length_m",
               "width_m",
               "height_m",
               "estimated_number_of_pieces")

  # fix missing values in individual jams
  # how many missing values are there?
  n_nas = jam_df %>%
    select(jam_cols) %>%
    is.na() %>%
    sum()

  if( fix_nas == TRUE & n_nas > 0) {
    cat("Imputing some missing values\n")

    fix_df = impute_missing_values(jam_df,
                                   col_nm_vec = jam_cols,
                                   ...)

    # make the estimated number of pieces an integer again
    if(class(fix_df$estimated_number_of_pieces) != "integer") {
      fix_df = fix_df %>%
        mutate_at(vars(estimated_number_of_pieces),
                  list(~ as.integer(round(.))))
    }

    jam_df = fix_df

  } # end if( fix_nas = TRUE )

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

