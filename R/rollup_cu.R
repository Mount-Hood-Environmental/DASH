#' @title Rollup Channel Unit Data
#'
#' @description Clean channel unit data (e.g., from `otg_type =` "CU_1.csv") and join
#' survey/site information to it
#'
#' @author Mike Ackerman, Kevin See, Richie Carmichael
#'
#' @param cu_df data.frame of `otg_type =` "CU_1.csv" containing data for each channel unit
#' @param survey_df data.frame of `otg_type =` "surveyPoint_0.csv" containing information for
#' each site/survey
#' @param fix_nas if any of the columns are still missing values i.e., `NA`, would you like to
#' fill them in? Default is `TRUE`, in which case the `NA` values will be imputed using the
#' function `impute_missing_values()`
#' @param impute_cols character vector of column names that should be imputed, if `fix_nas == TRUE`
#' @param ... other arguments to `impute_missing_values()`

#' @import dplyr
#' @export
#' @return a data.frame summarizing data for channel units

rollup_cu = function(cu_df = NULL,
                     survey_df = NULL,
                     fix_nas = TRUE,
                     impute_cols = c('maximum_depth_m'),
                     ...) {

} # end rollup_cu()




  stopifnot(!is.null(wood_df))

  # if any of the impute_cols are character vectors, turn them into factors
  cols_class = wood_df %>%
    select(any_of(impute_cols)) %>%
    sapply(class)
  if(sum(cols_class == "character") > 0) {
    wood_df = wood_df %>%
      mutate_at(vars(any_of(names(cols_class)[cols_class == "character"])),
                list(as.factor))
  }

  # how many missing values are there?
  n_nas = wood_df %>%
    select(any_of(impute_cols)) %>%
    is.na() %>%
    sum()

  # fix missing length and diameter values
  if( fix_nas == TRUE & n_nas > 0 ) {

    cat("Imputing some missing values\n")

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
    dplyr::summarise(lwd_length_m = sum(length_m),
                     lwd_diameter_m = sum(diameter_m),
                     lwd_area_m2 = sum(piece_area_m2),
                     lwd_vol_m3 = sum(piece_vol_m3),
                     lwd_n_pieces = length(parent_global_id),
                     # I should account for NA records in wet, channel_forming, ballasted here, somehow...TBD
                     lwd_n_wet = sum(wet == "Yes", na.rm = T),
                     lwd_n_chn_frm = sum(channel_forming == "Yes", na.rm = T),
                     lwd_n_ballast = sum(ballasted == "Yes", na.rm = T))

  return(return_df)

} # end rollup_cu_wood()
