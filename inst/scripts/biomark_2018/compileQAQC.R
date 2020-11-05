#' @title Compile QAQC errors
#'
#' @description Combine all of the QAQC errors from the dashQAQC() function across files within a directory
#'
#' @author Mike Ackerman
#'
#' @param folder_name character string of the folder name where list of folders with Survey123 data
#'
#' @import dplyr lubridate purrr
#' @return NULL
#' @export

compileQAQC = function(folder_name = NULL) {
  
  allQAQC = list.files(folder_name) %>%
    as.list() %>%
    rlang::set_names() %>%
    map_df(.id = 'folder',
           .f = function(x) {
             res = try(dashQAQC(paste(folder_name, x, sep = '/')))
             if(class(res)[1] == 'try-error') {
               cat(paste('Error with', x))
               return(NULL)
             } else {
               return(res)
             }
           })
}
