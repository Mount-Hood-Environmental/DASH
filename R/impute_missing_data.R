#' @title Impute Missing Data
#'
#' @description Filter out rows with too much missing data, impute the remaining missing values
#'
#' @author Kevin See
#'
#' @param data data.frame containing \code{covars} and any \code{impute_vars} as columns
#' @param covars vector of covariate names to be imputed, which match column names in \code{data}
#' @param max_missing integer value of the maximum number of columns per row that may have missing values. Rows with more than this number of missing covariates will be deleted.
#' @param impute_vars additional covariates included in \code{data} that may be used for imputation
#' @param method which method to use, either by creating random forests using the \code{missForest} package, or by predictive mean matching using the \code{aregImpute} function in the \code{Hmisc} package
#' @param ntree how many trees to build if \code{method = 'missForest'} or \code{method == 'randomForestSRC'}
#' @param nk how many knots to use in smoothing splines if using \code{method = 'Hmisc'}
#' @param ... other arguments to be passed to either \code{missForest}, \code{aregImpute} or \code{impute} functions
#'
#' @import dplyr tidyr missForest
#' @importFrom Hmisc aregImpute
#' @importFrom randomForestSRC impute
#' @return data.frame containing non-missing and imputed data
#' @export

impute_missing_data = function(data = NULL,
                               covars = NULL,
                               max_missing = 3,
                               impute_vars = NULL,
                               method = c('Hmisc', 'missForest', 'randomForestSRC'),
                               ntree = 100,
                               nk = 4,
                               ...) {
  stopifnot(exprs = {
    !is.null(data)
    !is.null(covars)
  })

  method = match.arg(method)
  
  # add row number to data
  data = data %>%
    mutate(row_num = 1:n())
  
  # filter out rows with too much missing data
  keep_rows = data %>%
    select(row_num, one_of(covars)) %>%
    mutate(nNA = rowSums(is.na(.))) %>%
    filter(nNA <= max_missing) %>%
    pull(row_num)
  
  all_cols = c(impute_vars, covars) %>% 
    unique()
  
  # imputed missing data with Hmisc package
  if(method == 'Hmisc') {
  set.seed(5)
  areg_data = data %>%
    filter(row_num %in% keep_rows) %>%
    select(one_of(all_cols)) %>%
    as.data.frame()
  
  for(i in which(sapply(areg_data, class) == 'factor')) {
    areg_data = areg_data %>%
      mutate_at(vars(i),
                list(fct_drop))
  }
  
  areg_data = areg_data %>%
    Hmisc::aregImpute(as.formula(paste('~', paste(colnames(.), collapse = '+'))),
                      data = .,
                      n.impute = 20,
                      nk = nk, ...)
  
  imputed_data = data %>%
    filter(row_num %in% keep_rows) %>%
    select(one_of(all_cols))
  for(colNm in all_cols) {
    if(!colNm %in% names(areg_data$imputed) |
       is.null(areg_data$imputed[[colNm]])) next
    if(class(pull(imputed_data, colNm)) == "integer") {
      imputed_data[as.numeric(rownames(areg_data$imputed[[colNm]])), colNm] = as.integer(round(rowMeans(areg_data$imputed[[colNm]])))
    } else {
      imputed_data[as.numeric(rownames(areg_data$imputed[[colNm]])), colNm] = rowMeans(areg_data$imputed[[colNm]])
    }
  }
  
  # pull out non-imputed data, combine with imputed data
  data_return = data %>%
    filter(row_num %in% keep_rows) %>%
    select(-one_of(names(imputed_data))) %>%
    bind_cols(imputed_data) %>%
    select(one_of(names(data)))  
  }
  
  # imputed missing data with missForest package
  if(method == 'missForest') {
    set.seed(5)
    imputed_data = data %>%
      filter(row_num %in% keep_rows) %>%
      select(one_of(all_cols)) %>%
      as.data.frame() %>%
      missForest::missForest(xmis = .,
                             variablewise = T,
                             verbose = F,
                             mtry = 10,
                             ntree = ntree, ...)
    
    
    # pull out non-imputed data, combine with imputed data
    data_return = data %>%
      filter(row_num %in% keep_rows) %>%
      select(-one_of(names(imputed_data$ximp))) %>%
      bind_cols(imputed_data$ximp) %>%
      select(one_of(names(data)))
  }
  
  # imputed missing data with randomForestSRC package
  if(method == 'randomForestSRC') {
    set.seed(5)
    imputed_data = data %>%
      filter(row_num %in% keep_rows) %>%
      select(one_of(all_cols)) %>%
      as.data.frame() %>%
      randomForestSRC::impute(data = .,
                              ntree = ntree,
                              ...) %>%
      as_tibble()
    
    # pull out non-imputed data, combine with imputed data
    data_return = data %>%
      filter(row_num %in% keep_rows) %>%
      select(-one_of(names(imputed_data))) %>%
      bind_cols(imputed_data) %>%
      select(one_of(names(data)))
  }
  
  # return imputed data set (minus rows with too much missing data)
  return(data_return)

}