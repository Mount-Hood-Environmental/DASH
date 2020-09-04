#' @title Fix Percents Sum
#'
#' @description This function re-scales the group of designated columns so their sum is 100%
#'
#' @author Kevin See
#'
#' @param data_df Input data.frame
#' @param col_names character vector of columns that should sum to 100
#' @param min_perc minimum sum of columns that should be re-scaled to 100
#' @param max_perc maximum sum of columns that should be re-scaled to 100
#'
#' @import dplyr
#' @export
#' @return a tibble with corrected percentages that sum to 100

rescale_percents <- function(data_df = NULL,
                             col_names = NULL,
                             min_perc = 90,
                             max_perc = 110) {
  stopifnot(!is.null(data_df),
            !is.null(col_names))

  fix_df = data_df %>%
    select(GlobalID,
           one_of(col_names)) %>%
    pivot_longer(cols = one_of(col_names)) %>%
    group_by(GlobalID) %>%
    mutate(sum_perc = sum(value, na.rm = T)) %>%
    ungroup() %>%
    # if all values sum to something greater than 0 & not every value is NA, change those NA values to 0
    mutate(value = if_else(sum_perc > 0 & is.na(value),
                           0,
                           value)) %>%
    group_by(GlobalID) %>%
    # if it appears that all entries are on decimal scale
    mutate(value = if_else(sum_perc < 2,
                           value * 100,
                           value)) %>%
    mutate(value = if_else(sum_perc == 10,
                           value * 10,
                           value)) %>%
    # redo sums
    group_by(GlobalID) %>%
    mutate(sum_perc = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = if_else(!sum_perc %in% c(0, 100) & between(sum_perc, min_perc, max_perc) & sum_perc%%10 == 0,
                           value / sum_perc * 100,
                           value)) %>%
    pivot_wider(names_from = "name",
                values_from = "value") %>%
    select(-sum_perc)

  return_df = data_df %>%
    select(-one_of(col_names)) %>%
    left_join(fix_df) %>%
    select(one_of(names(data_df)))

  return(return_df)

}
