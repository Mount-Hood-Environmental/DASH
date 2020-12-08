#' @title Compare Folders and Find Differences
#'
#' @description Compare two folders, each presumably containing the same files, except with
#' one folder containing files that have been modified or "fixed" by the user. For the `DASH`
#' R package, the function can be useful for comparing a folder containing raw untouched
#' on-the-ground (OTG) data from Survey123 with a folder containing the same files except modified
#' by the user, presumably in an attempt to remedy errors identified during data import or
#' QA/QC. Both `path1` and `path2` should only contain folders (no files) each representing
#' a survey, and each of those folders should only contain files (no folders) containing the otg
#' data.
#'
#' @author Mike Ackerman
#'
#' @param path1 a path to the first folder of interest, perhaps the raw untouched data. \code{path1}
#' should only contain folders (no files), and each of those folders should only contain files (no folders).
#' @param path2 a path to the second folder of interest, perhaps the data files that have been
#' modified or fixed by the user, presumably to remedy errors identified during data import or
#' QA/QC.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tools md5sum
#' @importFrom diffr diffr
#'
#' @export
#' @return fill out return when ready

compare_folders = function(path1 = ".",
                           path2 = ".") {

  # You need the suggested package for this function
  if(!requireNamespace("tools", quietly = T)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("diffr", quietly = T)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # files in path1
  files1 = get_file_nms(path1) %>%
    dplyr::select(path_nm) %>%
    as.list() %>%
    .[[1]]

  # files in path2
  files2 = get_file_nms(path2) %>%
    dplyr::select(path_nm) %>%
    as.list() %>%
    .[[1]]

  # check for differences in files1 and files2
  chk = files1[!(files1 %in% files2)]
  if(length(chk) > 0) cat(paste("The file", chk, "exists in path1, but not in path2. Continuing...", "\n"))

  chk = files2[!(files2 %in% files1)]
  if(length(chk) > 0) cat(paste("The file", chk, "exists in path2, but not in path1. Continuing...", "\n"))

  # final check, the lists of files should/need to be identical to proceed. Perhaps I can make this a little more
  # flexible at a later time/date
  chk = identical(files1, files2)
  if(chk == FALSE) stop(paste("path1 and path2 should contain the same number and names of files. Stopping execution.", "\n"))

  # create empty list for following for loop
  tmp_list = list()

  # if chk == TRUE, function will proceed
  # now loop over files and identify pairs of files btw path1 and path2 that contain differences
  for (f in files1) {

    # define files
    file1 = paste0(path1, f)
    file2 = paste0(path2, f)

    # test for differences
    test = tools::md5sum(file1) == tools::md5sum(file2)

    # if a difference is found, add to tmp_list
    if(test == FALSE) {
      tmp_list[[ f ]] = names(test)
      cat(paste("Found a difference in the", f, "files. Adding to the list.", "\n"))
    }

  } # end for loop 1

  # grab names from tmp_list
  tmp_list = names(tmp_list)

  # now loop over the files in tmp_list, find differences, and store them
  tmp_list2 = list()
  for (i in tmp_list) {

    # define files
    file1 = paste0(path1, i)
    file2 = paste0(path2, i)

    # use diffr to find differences
    diffr_tmp = diffr::diffr(file1, file2, before = "file1", after = "file2")

    # and store them in tmp_list2
    tmp_list2[[ i ]] = diffr_tmp

  } # end for loop 2

  tmp = list(diff_files  = tmp_list,
             differences = tmp_list2)
  return(tmp)

} # end compare_folders()
