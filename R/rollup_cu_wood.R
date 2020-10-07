#' @title Rollup Channel Unit Wood Data
#'
#' @description Summarize individual wood piece data (e.g. from \code{otg_type =} "Wood_2.csv") at
#' the channel unit scale
#'
#' @author Mike Ackerman and Richie Carmichael
#'
#' @param wood_df data.frame of \code{otg_type =} "Wood_2.csv" containing the individual
#' wood data to be summarized by channel units
#' @param fix_length_diam_nas if any of the length or diameter measurement for individual wood pieces
#' are missing (i.e, \code{NA}), would you like to fill them in? Default is `TRUE`. In this case, the NA
#' will be replaced using the median length or diameter from wood pieces in that channel unit.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export
#' @return a data.frame summarizing wood at the channel unit scale

rollup_cu_wood = function(wood_df = NULL,
                          fix_length_diam_nas = TRUE) {

  stopifnot(!is.null(wood_df))

  # store initial column names
  cols_wood_df = names(wood_df)

  # fix missing length and diameter values
  if( fix_length_diam_nas == TRUE ) {

    # na_cus = wood_df %>%
    #   select(parent_global_id,
    #          length_m,
    #          diameter_m) %>%
    #   filter(is.na(length_m) | is.na(diameter_m)) %>%
    #   select(parent_global_id) %>%
    #   pull()

    fixed_l_d = wood_df %>%
      dplyr::select(parent_global_id,
                    global_id,
                    length_m,
                    diameter_m) %>%
      tidyr::pivot_longer(cols = c(length_m,
                                   diameter_m)) %>%
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

    wood_df = wood_df %>%
      dplyr::select(-length_m,
                    -diameter_m) %>%
      dplyr::left_join(fixed_l_d) %>%
      dplyr::select(one_of(cols_wood_df))

  } # end if( fix_length_diam_nas = TRUE )

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
