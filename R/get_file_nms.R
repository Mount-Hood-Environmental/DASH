#' @title Get File Names
#'
#' @description Create a data frame of folders and file names within `path`
#'
#' @author Mike Ackerman and Kevin See
#'
#' @param path A path to the directory containing the folders of interest. `path` should only contain
#' folders (no files), and each of those folders should only contain files (no folders).
#'
#' @import purrr dplyr
#' @importFrom utils stack
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @export
#' @return a data frame of all files contained in `path`

get_file_nms = function(path = ".") {

  # list the folders in path
  folder_nms = list.files(path)

  # if we want folder_list argument later
  # if folder_list is defined, trim folder_nms to only include those i.e. allow user to define list of folders
  # if(!is.null(folder_list)) {
  #   folder_nms = folder_nms[folder_nms %in% folder_list]
  # }

  # return error if path contains no folders
  if(length(folder_nms) == 0) stop("No folders were found in path.")

  # create list folder
  folders = as.list(folder_nms)
  names(folders) = folder_nms

  file_df = folders %>%
    purrr::map(.f = function(x) {
      list.files(paste(path, x[1], sep = "/"))
    }) %>%
    utils::stack() %>%
    dplyr::select(folder_nm = ind,
                  file_nm = values) %>%
    #dplyr::tbl_df() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(path_nm = paste(folder_nm, file_nm, sep = "/"))

  return(file_df)

} # end get_file_nms()
