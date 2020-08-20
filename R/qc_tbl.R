#' @title Initiate Blank QA/QC Results Tibble
#'
#' @description Create a blank tibble to store messages from QA/QC of on-the-ground
#' collected data
#'
#' @author Mike Ackerman
#'
#' @importFrom tibble tibble
#' @export
#' @return a blank tibble

qaqc_tbl = function() {

  # create a blank tibble
  tmp = tibble::tibble(
    path_name = character(),
    GlobalID = integer(),
    error_message = character()
  )

  return(tmp)

} # end qaqc_tbl()
