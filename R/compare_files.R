#' @title Compare Files and Find Differences
#'
#' @description Compare two files, each presumably containing the same data, except with
#' one folder containing files that have been modified or "fixed" by the user. For the `DASH`
#' R package, the function can be useful for comparing a file containing raw untouched
#' on-the-ground (OTG) data from Survey123 with a file containing the same data except modified
#' by the user, presumably in an attempt to remedy errors identified during data import or
#' QA/QC. Both `file1` and `file2` should only contain entire paths, including file name and extension, each representing
#' a survey containing the otg data. These files must be either .csv or .xlsx/.xls files.
#'
#' @author Kevin See
#'
#' @param file1 a path to the first file of interest, including file extension, perhaps the raw untouched data.
#' @param file2 a path to the second file of interest, including file extension, perhaps the data files that have been
#' modified or fixed by the user, presumably to remedy errors identified during data import or
#' QA/QC.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#'
#' @export
#' @return fill out return when ready

compare_files = function(file1 = NA,
                         file2 = NA,
                         ...) {

  stopifnot(!is.na(file1),
            !is.na(file2))

  # read in first file
  if(grepl('csv$', file1)) {
    df1 = suppressMessages(readr::read_csv(file1)) %>%
      janitor::clean_names()
  } else if(grepl('xlsx$', file1) | grepl('xls', file1)) {
    df1 = suppressMessages(readxl::read_excel(file1,
                                              ...)) %>%
      janitor::clean_names()
  } else {
    stop("File extension for file1 is not .csv or .xlsx")
  }

  # read in second file
  if(grepl('csv$', file2)) {
    df2 = suppressMessages(readr::read_csv(file2)) %>%
      janitor::clean_names()
  } else if(grepl('xlsx$', file2) | grepl('xls', file2)) {
    df2 = suppressMessages(readxl::read_excel(file2,
                                              ...)) %>%
      janitor::clean_names()
  } else {
    stop("File extension for file2 is not .csv or .xlsx")
  }

  if(!setequal(names(df1), names(df2))) {
    stop("Column names don't match")
  }

  # pull out differences in numeric columns
  num_diff = df1 %>%
    select(-object_id,
           -any_of(c("parent_global_id",
                     "creation_date",
                     "creator",
                     "edit_date",
                     "editor"))) %>%
    select(global_id, where(is.numeric)) %>%
    pivot_longer(cols = -global_id,
                 names_to = "metric") %>%
    mutate(source = "file1") %>%
    bind_rows(df2 %>%
                select(-object_id,
                       -any_of(c("parent_global_id",
                                 "creation_date",
                                 "creator",
                                 "edit_date",
                                 "editor"))) %>%
                select(global_id, where(is.numeric)) %>%
                pivot_longer(cols = -global_id,
                             names_to = "metric") %>%
                mutate(source = "file2")) %>%
    pivot_wider(names_from = "source",
                values_from = "value") %>%
    filter(file1 != file2 |
             (is.na(file1) & !is.na(file2)) |
             (is.na(file2) & !is.na(file1)))

  # pull out differences in character or factor columns
  chr_diff = df1 %>%
    select(-object_id,
           -any_of(c("parent_global_id",
                     "creation_date",
                     "creator",
                     "edit_date",
                     "editor"))) %>%
    select(global_id, where(is.character), where(is.factor)) %>%
    mutate(across(where(is.factor),
                  as.character))
  if(ncol(chr_diff) > 1) {
    chr_diff = chr_diff %>%
    pivot_longer(cols = -global_id,
                 names_to = "metric") %>%
    mutate(source = "file1") %>%
    bind_rows(df2 %>%
                select(-object_id,
                       -any_of(c("parent_global_id",
                                 "creation_date",
                                 "creator",
                                 "edit_date",
                                 "editor"))) %>%
                select(global_id, where(is.character), where(is.factor)) %>%
                mutate(across(where(is.factor),
                              as.character)) %>%
                pivot_longer(cols = -global_id,
                             names_to = "metric") %>%
                mutate(source = "file2")) %>%
    pivot_wider(names_from = "source",
                values_from = "value") %>%
    filter(file1 != file2 |
             (is.na(file1) & !is.na(file2)) |
             (is.na(file2) & !is.na(file1)))
  } else {
    chr_diff = tibble(global_id = NA_character_,
                      metric = NA_character_) %>%
      slice(0)
  }

  # put them together in a list
  res = list(num_diff = num_diff,
             chr_diff = chr_diff)

  # get some info about the files
  source = stringr::str_split(file1, "/") %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    slice(nrow(.)) %>%
    pull(value) %>%
    stringr::str_extract(., "[:alpha:]+")

  # add that info to the results
  res = res %>%
    map(.f = function(x) {
      if(!is.null(x)) {
        x = x %>%
          tibble::add_column(source = source) %>%
          select(source,
                 everything())
        return(x)
      }
    })

  return(res)

} # end compare_files()
