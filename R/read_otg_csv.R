#' @title Read On-The-Ground .csv Data
#'
#' @description Read .csv delimited file containing on-the-ground (otg) data collected using the
#' DASH protocol. This function only reads in one "type" of data at a time.
#'
#' @author Mike Ackerman and Kevin See
#'
#' @inheritParams get_file_nms
#'
#' @param otg_type what type of on-the-ground (otg) data would the user like to read? As of the iniitation of the
#' function a character vector of the file name e.g., "surveyPoint_0.csv", "CU_1.csv", etc.
#'
#' @import purrr dplyr
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @export
#' @return a data frame containing data of \code{otg_type}

read_otg_csv = function(path = ".",
                        otg_type = NULL) {

  # files list of the otg_type
  file_list = get_file_nms(path) %>%
    dplyr::filter(file_nm == otg_type) %>%
    dplyr::mutate(file_cnt = row_number()) %>%
    split(list(.$path_nm))

  # name of the first file of otg_type
  first_otg_file = paste0(path, file_list[[1]]$path_nm)

  # extract column specifications from the first file
  first_spec = readr::read_csv(first_otg_file) %>%
    spec()

  tmp_df = file_list %>%
    purrr::map_df(.id = "path_nm",
                  .f = function(x) {

                    cat(paste("Reading", otg_type, "file from", x$folder_nm, "survey.", "\n"))

                    # read in each .csv file of otg_type
                    tmp = try(suppressWarnings(readr::read_csv(paste0(path, x$path_nm),
                                                               col_types = cols())))

                    # ERROR MESSAGES

                    # if nrow of tmp (a single file) is 0 or class(tmp) is error
                    if(nrow(tmp) == 0 | class(tmp)[1] == "try-error") {
                      cat(paste("Problem reading in", otg_type, "file from", x$folder_nm, "survey.", "\n"))
                      return(NULL)
                    }

                    # check that column specifications of tmp match those from the first file
                    check = identical(first_spec, spec(tmp))
                    if(check == FALSE) stop(paste("Bummer. Column specifications for", otg_type, "file from", x$folder_nm, "survey.", "\n",
                                                  "do not match those in", first_otg_file, ". \n",
                                                  "Please check your file formats and try again."))

                    return(tmp)
                  })

  return(tmp_df)
} # end read_otg_csv()
