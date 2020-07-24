#' @title Read On-The Ground .csv Data - Wrapper
#'
#' @description A wrapper function for \code{read_otg_csv()}. Read .csv delimited files containing
#' on-the-ground (otg) data collected using the DASH protocol. This function can loop over
#' several (perhaps all) "types" of otg data at once to create a list of data frames, each
#' containing data for one of the data types.
#'
#' @author Mike Ackerman
#'
#' @inheritParams get_file_nms
#' @inheritParams get_otg_col_specs
#'
#' @param otg_type_names an optional vector used to name each data frame of \code{otg_type_list}.
#' If \code{otg_type_names} argument is specified, it MUST be the same length as \code{otg_type_list}.
#' If the default \code{otg_type_names = NULL} is used, data frames will be names using
#' the \code{otg_type_list} (i.e., the file names).
#'
#' @export
#' @return a list of data frames, each containing data from \code{otg_type_list}

read_otg_csv_wrapper = function(path = ".",
                                otg_type_list = c("surveyPoint_0.csv",
                                                  "CU_1.csv",
                                                  "Wood_2.csv",
                                                  "Jam_3.csv",
                                                  "Undercut_4.csv",
                                                  "Discharge_5.csv",
                                                  "DischargeMeasurements_6.csv"),
                                otg_type_names = NULL) {

  # if otg_type_names if NOT NULL, verify it is the same length as otg_type_list
  if(!is.null(otg_type_names)) {
    chk = identical(length(otg_type_list), length(otg_type_names))
    if(chk == FALSE) stop("If argument otg_type_names is not NULL, otg_type_list and otg_type_names MUST be of the same length. Stopping execution")
  }

  # create empty list
  df_list = list()

  # initiate counter
  ctr = 1

  # for loop over otg_type_list
  for (o in otg_type_list) {

    tmp = read_otg_csv(path = path,
                       otg_type = o)

    # add tmp df to otg_data list, assign a new name if otg_type_names is not NULL
    if(!is.null(otg_type_names)) {
      df_list[[ otg_type_names[ctr] ]] = assign(otg_type_names[ctr], tmp)
    } else {
      df_list[[ o ]] = assign(o, tmp)
    }

    ctr = ctr + 1

  } # end for loop

  return(df_list)

} # end read_otg_csv_wrapper()
