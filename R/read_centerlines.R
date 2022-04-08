#' Read In Centerlines
#' @description Import centerline shapefiles (.shp). The centerlines should
#' contain, at the least, the variable names "site_name", "year", and "cu_num"
#'
#' @author Mike Ackerman
#'
#' @param cl_names The file name of each centerline. All centerlines should share
#' same name, but be saved in separate folders for each site.
#' @param find_duplicates Would you like to search for duplicate channel units
#' within the centerlines after import?
#'
#' @inheritParams get_file_nms
#'
#' @import dplyr purrr
#' @importFrom rlang set_names
#' @importFrom stringr str_remove
#' @importFrom sf st_read
#' @importFrom janitor clean_names
#' @importFrom tidyr unite
#' @export
#'
#' @return a shapefile of merged centerlines

read_centerlines = function(path = ".",
                            cl_names = "centerlines.shp",
                            find_duplicates = TRUE) {

  # get path name to all centerline shapefiles
  cl_files = list.files(path,
                        pattern = paste0(cl_names, "$"),
                        recursive = TRUE) %>%
    as.list() %>%
    rlang::set_names(nm = function(x) stringr::str_remove(x,
                                                          paste0("/", cl_names, "$"))) %>%
    purrr::map(.f = function(x) {
      paste(path, x, sep = "/")
    })

  # the columns we're interested in from the centerline shapefiles
  cl_cols = c("path_nm",
              "site_name",
              "year",
              "cu_num",
              "geometry")

  # read in all cl_files
  cl_list = cl_files %>%
    purrr::map(.f = function(x) {
      sf::st_read(x) %>%
        dplyr::mutate(path_nm = stringr::str_remove(x, path)) %>%
        janitor::clean_names() %>%
        dplyr::select(path_nm,
                      everything())
    }) %>%
    purrr::map(.f = function(x) {
      if(sum(!cl_cols %in% names(x)) > 0) {
        for(cl_cols in cl_cols[!cl_cols %in% names(x)]) {
          x[,cl_cols] = NA_character
        }
      }
      x %>%
        select(any_of(cl_cols))
    })

  # merge cl_list into a single sf object
  cl_sf = purrr::map_df(cl_list,
                        .f = identity) %>%
    # add an object_id column
    dplyr::mutate(object_id = 1:n()) %>%
    dplyr::select(object_id,
                  path_nm,
                  everything())

  # look for duplicates in centerlines if find_duplicates = TRUE
  if( find_duplicates == TRUE ) {
    dup_cl_cus = cl_sf %>%
      tidyr::unite(col = cu_id,
                   site_name, year, cu_num,
                   remove = FALSE) %>%
      dplyr::filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
      sf::st_drop_geometry() %>%
      pull(cu_id)

    if( length(dup_cl_cus) == 0 ) cat("No duplicate channel units found within centerlines. \n")
    if( length(dup_cl_cus) > 0 ) {
      cat("Found duplicate channel units within centerlines. \n")
      print(dup_cl_cus)
     }
  } # end find_duplicates

  return(cl_sf)

} # end read_centerlines()
