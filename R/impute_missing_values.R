#' @title Impute Missing Values
#'
#' @description Impute any missing values from selected columns within a data.frame
#'
#' @author Kevin see
#'
#' @param data_df data.frame with potential missing values
#' @param col_nm_vec character vector of column names that should be either imputed and/or used to impute other columns
#' @param method which method to use, either by creating random forests using the `missForest` package, or by predictive mean matching using the `aregImpute()` function in the `Hmisc` package
#' @param my_seed seed for random number generator, to make results reproducible
#' @param ntree how many trees to build if `missForest` or `randomForestSRC`
#' @param nk how many knots to use in smoothing splines if using `method = 'Hmisc'`
#' @param ... other arguments to be passed to either `missForest::missForest()`, `Hmisc::aregImpute()`, or `randomForestSRC::impute()` functions
#'
#' @import dplyr
#' @importFrom Hmisc aregImpute
#' @importFrom randomForestSRC impute
#' @importFrom missForest missForest
#' @importFrom forcats fct_drop
#' @importFrom tibble as_tibble
#' @export
#' @return a data.frame with the same dimensions as the original, but with no missing values

impute_missing_values = function(data_df = NULL,
                                 col_nm_vec = NULL,
                                 method = c('randomForestSRC', 'missForest', 'Hmisc'),
                                 my_seed = 5,
                                 ntree = 1000,
                                 nk = 4,
                                 ...) {
  stopifnot(!is.null(data_df))

  # if col_nm_vec is not provided, use all columns of data_df
  if(is.null(col_nm_vec)) {
    col_nm_vec = names(data_df)
  }

  method = match.arg(method)

  # imputed missing data with randomForestSRC package
  if(method == "randomForestSRC") {
    set.seed(my_seed)
    imputed_data = data_df %>%
      dplyr::select(dplyr::any_of(col_nm_vec)) %>%
      as.data.frame() %>%
      randomForestSRC::impute(data = .,
                              ntree = ntree,
                              ...) %>%
      tibble::as_tibble()

    data_return = data_df %>%
      dplyr::select(-dplyr::any_of(names(imputed_data))) %>%
      dplyr::bind_cols(imputed_data) %>%
      dplyr::select(dplyr::any_of(names(data_df)))
  } # end if randomForestSRC

  # imputed missing data with missForest package
  if(method == 'missForest') {
    set.seed(my_seed)
    imputed_data = data_df %>%
      dplyr::select(dplyr::any_of(col_nm_vec)) %>%
      as.data.frame() %>%
      missForest::missForest(xmis = .,
                             # variablewise = T,
                             # verbose = F,
                             ntree = ntree,
                             ...)


    # pull out non-imputed data, combine with imputed data
    data_return = data_df %>%
      dplyr::select(-dplyr::any_of(names(imputed_data$ximp))) %>%
      dplyr::bind_cols(imputed_data$ximp) %>%
      dplyr::select(dplyr::any_of(names(data_df)))
  } # end if missForest

  # imputed missing data with Hmisc package
  if(method == 'Hmisc') {
    set.seed(my_seed)
    areg_data = data_df %>%
      dplyr::select(dplyr::any_of(col_nm_vec)) %>%
      as.data.frame()

    for(i in which(sapply(areg_data, class) == 'factor')) {
      areg_data = areg_data %>%
        dplyr::mutate_at(vars(dplyr::all_of(i)),
                         list(forcats::fct_drop))
    }

    areg_data = areg_data %>%
      Hmisc::aregImpute(as.formula(paste('~', paste(colnames(.), collapse = '+'))),
                        data = .,
                        n.impute = 20,
                        nk = nk,
                        ...)

    imputed_data = data_df %>%
      dplyr::select(dplyr::any_of(col_nm_vec))

    for(colNm in col_nm_vec) {
      if(!colNm %in% names(areg_data$imputed) |
         is.null(areg_data$imputed[[colNm]])) next
      if(class(dplyr::pull(imputed_data, colNm)) == "factor") {
        imputed_data[as.numeric(rownames(areg_data$imputed[[colNm]])), colNm] = levels(dplyr::pull(imputed_data, colNm))[apply(areg_data$imputed[[colNm]], 1, median)]
      } else if(class(dplyr::pull(imputed_data, colNm)) == "integer") {
        imputed_data[as.numeric(rownames(areg_data$imputed[[colNm]])), colNm] = as.integer(round(rowMeans(areg_data$imputed[[colNm]])))
      } else {
        imputed_data[as.numeric(rownames(areg_data$imputed[[colNm]])), colNm] = rowMeans(areg_data$imputed[[colNm]])
      }
    }

    # pull out non-imputed data, combine with imputed data
    data_return = data_df %>%
      dplyr::select(-dplyr::any_of(names(imputed_data))) %>%
      dplyr::bind_cols(imputed_data) %>%
      dplyr::select(dplyr::any_of(names(data_df)))

  } # end if Hmisc

  # return imputed data set
  return(data_return)

}
