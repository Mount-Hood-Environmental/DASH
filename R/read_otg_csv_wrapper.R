#' @title Read On-The-Ground .csv Data - Wrapper
#'
#' @description A wrapper function for `read_otg_csv()`. Import .csv delimited files
#' containing on-the-ground (OTG) data collected using the DASH protocol. This function
#' can be performed on multiple (perhaps all) "types" of OTG data at once to create a
#' list of data frames, each containing data for one of the data types.
#'
#' @author Mike Ackerman
#'
#' @inheritParams get_file_nms
#' @inheritParams get_otg_col_specs
#'
#' @param otg_type_names an optional character vector that can be used to name each
#' data frame of `otg_type_list`. If the `otg_type_names` argument is provided, it
#' MUST be the same length as `otg_type_list`. If the default `otg_type_names = NULL`
#' is used, data frames will be named using the `otg_type_list` character
#' vector (i.e., the .csv file names).
#'
#' @export
#' @return a list of data frames, each containing data from `otg_type_list`

read_otg_csv_wrapper = function(path = ".",
                                otg_type_list = c("surveyPoint_0.csv",
                                                  "CU_1.csv",
                                                  "Wood_2.csv",
                                                  "Jam_3.csv",
                                                  "Undercut_4.csv",
                                                  "Discharge_5.csv"),
                                otg_type_names = NULL) {

  # if otg_type_names is NOT NULL, verify it is the same length as otg_type_list
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

