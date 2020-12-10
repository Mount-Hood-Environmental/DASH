#' @title Initiate Blank QC Results Tibble
#'
#' @description Create a blank tibble to store messages from QA/QC of on-the-ground
#' collected data
#'
#' @author Mike Ackerman
#'
#' @param id name of identifying column
#'
#' @importFrom tibble tibble
#' @export
#' @return a blank tibble

qc_tbl = function(data_id = "GlobalID") {

  # create a blank tibble
  tmp = tibble::tibble(
    path_nm = character(),
    id = character(),
    error_message = character()
  )
  names(tmp)[match("id", names(tmp))] = data_id

  return(tmp)

} # end qc_tbl()
