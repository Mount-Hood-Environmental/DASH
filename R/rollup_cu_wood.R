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
    dplyr::mutate(piece_area_m2 = diameter_m * length_m,
                  piece_vol_m3 = pi * (diameter_m/2)^2 * length_m) %>%
    dplyr::group_by(parent_global_id) %>%
    dplyr::summarise(lwd_n_pieces = length(parent_global_id),
                     #lwd_length_m = sum(length_m),
                     #lwd_diameter_m = sum(diameter_m),
                     lwd_area_m2 = sum(piece_area_m2),
                     lwd_vol_m3 = sum(piece_vol_m3),
                     # calculate proportions w/in channel units for wet, channel_forming, and ballasted
                     # ignore NAs when calculating proportions
                     lwd_p_wet = sum(wet == "Yes", na.rm = T) / sum(wet == "Yes" | wet == "No", na.rm = T),
                     lwd_p_chn_frm = sum(channel_forming == "Yes", na.rm = T) / sum(channel_forming == "Yes" | channel_forming == "No", na.rm = T),
                     lwd_p_ballast = sum(ballasted == "Yes", na.rm = T) / sum(ballasted == "Yes" | ballasted == "No", na.rm = T))


  return(return_df)

} # end rollup_cu_wood()
