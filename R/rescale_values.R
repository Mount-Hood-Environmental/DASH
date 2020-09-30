#' @title Re-scale Values
#'
#' @description Re-scale a group of designated columns so their sum equals \code{sum_to}
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param data_df input data.frame
#' @param col_names character vector of columns that should sum to \code{sum_to}
#' @param sum_to the value that the columns should sum to
#' @param min_perc minimum sum of columns that should be re-scaled to \code{sum_to}
#' @param max_perc maximum sum of columns that should be re-scaled to \code{sum_to}
#'
#' @import dplyr
#' @export
#' @return a tibble with corrected percentages that sum to 100

rescale_values <- function(data_df = NULL,
                           col_names = NULL,
                           sum_to = 100,
                           min_value = 80,
                           max_value = 120) {
  stopifnot(!is.null(data_df),
            !is.null(col_names))

  fix_df = data_df %>%
    select(GlobalID,
           one_of(col_names)) %>%
    pivot_longer(cols = one_of(col_names)) %>%
    group_by(GlobalID) %>%
    mutate(sum_values = sum(value, na.rm = T)) %>%
    ungroup() %>%
    # if all values for a channel unit sum to something greater than 0 & not every value is NA, change those NA values to 0
    mutate(value = if_else(sum_values > 0 & is.na(value),
                           0,
                           value)) %>%
    # rescale any values between min_value & max_value to equal sum_to
    mutate(value = if_else(!sum_values %in% c(0, sum_to) & between(sum_values, min_value, max_value),
                           value / sum_values * sum_to, # the re-scaling!
                           value)) %>%
    pivot_wider(names_from = "name",
                values_from = "value") %>%
    select(-sum_values)

  return_df = data_df %>%
    select(-one_of(col_names)) %>%
    left_join(fix_df) %>%
    select(one_of(names(data_df)))

  return(return_df)

} # end rescale_values()
