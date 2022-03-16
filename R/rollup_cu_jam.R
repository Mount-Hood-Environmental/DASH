#' @title Rollup Channel Unit Jam Data
#'
#' @description Summarize individual jam data (e.g., from `otg_type =` "Jam_3.csv") at
#' the channel unit scale
#'
#' @author Mike Ackerman, Richie Carmichael, and Kevin See
#'
#' @param jam_df data.frame of `otg_type =` "Jam_3.csv containing the individual
#' jam data to be summarized (rolled up) to the channel unit scale
#' @param fix_nas if any of the length, width, height, or estimated number of pieces measurements
#' for an individual jam is missing i.e.,`NA`, would you like to fill them in? Default is `TRUE`, in which
#' case the `NA` values will be imputed using function `impute_missing_values()`
#' @param impute_cols character vector of column names that should be imputed, if `fix_nas == TRUE`
#' @param ... other arguments to `impute_missing_values()`
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing jam data at the channel unit scale

rollup_cu_jam = function(jam_df = NULL,
                         fix_nas = TRUE,
                         impute_cols  = c("length_m",
                                          "width_m",
                                          "height_m",
                                          "estimated_number_of_pieces"),
                         ...) {

  stopifnot(!is.null(jam_df))

  # not necessary for jams, shouldn't be any character vectors
  # # get class of each impute_cols
  # cols_class = jam_df %>%
  #   dplyr::select(dplyr::any_of(impute_cols)) %>%
  #   sapply(class)
  #
  # # if any impute_cols are character vectors, turn them into factors
  # if(sum(cols_class == "character") > 0) {
  #   jam_df = jam_df %>%
  #     dplyr::mutate_at(vars(dplyr::any_of(names(cols_class)[cols_class == "character"])),
  #                      list(as.factor))
  # }

  # how many missing values are there in impute_cols?
  n_nas = jam_df %>%
    dplyr::select(dplyr::any_of(impute_cols)) %>%
    is.na() %>%
    sum()

  if( fix_nas == TRUE & n_nas == 0 ) cat("No missing values in impute_cols of jam_df\n")

  # fix missing values in individual jams
  if( fix_nas == TRUE & n_nas > 0) {

    cat("Imputing some missing values in jam_df\n")

    fix_df = impute_missing_values(jam_df,
                                   col_nm_vec = impute_cols,
                                   ...)

    # make the estimated number of pieces an integer again
    if(class(fix_df$estimated_number_of_pieces) != "integer") {
      fix_df = fix_df %>%
        dplyr::mutate_at(vars(estimated_number_of_pieces),
                         list(~ as.integer(round(.))))
    }

    jam_df = fix_df

  } # end if( fix_nas == TRUE & n_nas > 0 ) loop

  return_df = jam_df %>%
    dplyr::select(-(creation_date:editor)) %>%
    dplyr::mutate(area_m2 = length_m * width_m,
                  vol_m3 = length_m * width_m * height_m) %>%
    dplyr::group_by(parent_global_id) %>%
    dplyr::summarise(jam_n = length(parent_global_id),
                     jam_est_n_pieces = sum(estimated_number_of_pieces),
                     jam_area_m2 = sum(area_m2),
                     jam_vol_m3 = sum(vol_m3))

  return(return_df)

} # end rollup_cu_jam()

