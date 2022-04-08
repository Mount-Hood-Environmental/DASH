#' @title Rollup Channel Unit Wood Data
#'
#' @description Summarize individual wood piece data (e.g. from \code{otg_type =} "Wood_2.csv") at
#' the channel unit scale
#'
#' @author Mike Ackerman, Richie Carmichael, and Kevin See
#'
#' @param wood_df data.frame of `otg_type =` "Wood_2.csv" containing the individual
#' wood data to be summarized by channel units
#' @param fix_nas if any of the length or diameter measurement for individual wood pieces are missing
#' i.e.,`NA`, would you like to fill them in? Default is `TRUE`, in which
#' case the `NA` values will be imputed using function `impute_missing_values()`
#' @param impute_cols character vector of column names that should be imputed, if `fix_nas == TRUE`
#' @param ... other arguments to `impute_missing_values()`
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing wood at the channel unit scale

rollup_cu_wood = function(wood_df = NULL,
                          fix_nas = TRUE,
                          impute_cols = c('length_m',
                                          'diameter_m'),
                          ...) {

  stopifnot(!is.null(wood_df))

  # get class of each impute_cols
  cols_class = wood_df %>%
    dplyr::select(dplyr::any_of(impute_cols)) %>%
    sapply(class)

  # if any impute_cols are character vectors, turn them into factors
  if(sum(cols_class == "character") > 0) {
      wood_df = wood_df %>%
        dplyr::mutate_at(vars(dplyr::any_of(names(cols_class)[cols_class == "character"])),
                         list(as.factor))
  }

  # how many missing values are there in impute_cols?
  n_nas = wood_df %>%
    dplyr::select(dplyr::any_of(impute_cols)) %>%
    is.na() %>%
    sum()

  if( fix_nas == TRUE & n_nas == 0 ) cat("No missing values in impute_cols of wood_df\n")

  # fix missing length and diameter values
  if( fix_nas == TRUE & n_nas > 0 ) {

    cat("Imputing some missing values in wood_df\n")

    # use default values
    fix_df = impute_missing_values(wood_df,
                                   col_nm_vec = impute_cols,
                                   ...)

    wood_df = fix_df

  } # end if( fix_nas == TRUE & n_nas > 0 ) loop

  # now start data rollup
  return_df = wood_df %>%
    dplyr::select(-(creation_date:editor)) %>%
    dplyr::mutate(piece_area_m2 = length_m * diameter_m,
                  piece_vol_m3 = length_m * (diameter_m/2)^2 * pi) %>%
    dplyr::group_by(parent_global_id) %>%
    dplyr::summarise(lwd_n = length(parent_global_id),
                     lwd_area_m2 = sum(piece_area_m2),
                     lwd_vol_m3 = sum(piece_vol_m3),
                     lwd_n_wet = length(parent_global_id[wet == "Yes"]),
                     lwd_area_wet_m2 = sum(piece_area_m2[wet == "Yes"]),
                     lwd_vol_wet_m3 = sum(piece_vol_m3[wet == "Yes"]),
                     lwd_n_cf = length(parent_global_id[channel_forming == "Yes"]),
                     lwd_area_cf_m2 = sum(piece_area_m2[channel_forming == "Yes"]),
                     lwd_vol_cf_m3 = sum(piece_vol_m3[channel_forming == "Yes"]),
                     lwd_n_bal = length(parent_global_id[ballasted == "Yes"]),
                     lwd_area_bal_m2 = sum(piece_area_m2[ballasted == "Yes"]),
                     lwd_vol_bal_m3 = sum(piece_vol_m3[ballasted == "Yes"])) %>%
    dplyr::mutate_at(vars(ends_with("_m2")), round, 2) %>%
    dplyr::mutate_at(vars(ends_with("_m3")), round, 2)

  return(return_df)

} # end rollup_cu_wood()
