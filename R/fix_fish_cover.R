#' @title Fix Fish Cover Columns
#'
#' @description Fixes typical issues identified in the fish cover columns of cu_df including
#' re-scaling estimates entered as proportions into percentages and re-scaling any estimates that
#' sum to 90 (likely due to a math error in the field) instead to 100. Any remaining issues (e.g.,
#' values sum outside the "expected" 100 - 130 or estimates are a combination of proportions and
#' percentages) should be identified by the `qc_cu()` function.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param cu_df data.frame of `otg_type =` "CU_1.csv" data that should contain
#' the \code{cover_cols} to be fixed.
#' @param cover_cols character vector of fish cover column names
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export
#' @return a tibble with some fish cover estimates resolved

fix_fish_cover <- function(cu_df = NULL,
                           cover_cols = c("Overhanging Cover",
                                          "Aquatic Vegetation",
                                          "Woody Debris Cover",
                                          "Artificial Cover",
                                          "Total No Cover")) {
  stopifnot(!is.null(cu_df))

  fix_df = cu_df %>%
    dplyr::select(GlobalID,
                  one_of(cover_cols)) %>%
    tidyr::pivot_longer(cols = one_of(cover_cols)) %>%
    dplyr::group_by(GlobalID) %>%
    dplyr::mutate(sum_values = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # if all values for a channel unit sum to something greater than 0 & not every value is NA,
    # change those NA values to 0...
    dplyr::mutate(value = dplyr::if_else(sum_values > 0 & is.na(value),
                                         0,
                                         value)) %>%
    # remove records where values sum to 0, we're going to leave those as NA (i.e., not touch them)
    dplyr::filter(!sum_values == 0) %>%
    # if all values sum between 0.9 and 1.9 and appear to be proportions (decimal scale), convert them to
    # percents before moving on...
    dplyr::group_by(GlobalID) %>%
    dplyr::mutate(value = dplyr::if_else(sum_values < 2,
                                         value * 100,
                                         value)) %>%
    # redo sums
    dplyr::group_by(GlobalID) %>%
    dplyr::mutate(sum_values = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # re-scale any that sum to 90, up to 100; we're confident these are likely math errors in the field
    dplyr::group_by(GlobalID) %>%
    dplyr::mutate(value = dplyr::if_else(sum_values == 90,
                                         value / sum_values * 100,
                                         value)) %>%
    # redo sums, again
    dplyr::group_by(GlobalID) %>%
    dplyr::mutate(sum_values = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # finally, re-structure the df
    tidyr::pivot_wider(names_from = "name",
                       values_from = "value") %>%
    dplyr::select(-sum_values)

  # and create the return df
  return_df = cu_df %>%
    dplyr::select(-one_of(cover_cols)) %>%
    dplyr::left_join(fix_df) %>%
    dplyr::select(one_of(names(cu_df)))

  return(return_df)

} # end fix_fish_cover()
