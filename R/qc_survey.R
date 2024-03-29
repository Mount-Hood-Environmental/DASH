#' @title Quality Control - Survey Data
#'
#' @description Quality control survey data (e.g., from surveyPoint_0.csv files) imported
#' using `read_otg_csv()` or `read_otg_csv_wrapper()`.
#'
#' @author Mike Ackerman
#'
#' @param qc_df The survey data frame to be QC'd
#' @inheritParams check_na
#'
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @export
#' @return a tibble with QC results

qc_survey = function(qc_df = NULL,
                     cols_to_check_nas = c("Stream Name",
                                           "Site Name",
                                           "Survey Start Date Time",
                                           "Survey Crew",
                                           "Water Temp (C)",
                                           "Conductivity (ms)")) {
  stopifnot(!is.null(qc_df))

  # set otg_type
  otg_type = "surveyPoint_0.csv"

  # Starting message
  cat(paste("Starting QC on otg_type =", otg_type, ". \n"))

  # Initiate qc_tmp
  qc_tmp = qc_tbl()

  #####
  # CHECK 1: Do the column names in qc_df match the expected column names for that otg_type?
  check_col_names(qc_df = qc_df,
                  otg_type = otg_type)

  #####
  # CHECK 2: Are there duplicate site names?
  cat(paste("Looking for duplicate site names... \n"))

  #dup_site = c(qc_df$`Site Name`, "Arbon_2019", "Canyon_2019") %>% # for testing
  dup_site = qc_df$`Site Name` %>%
    table() %>%
    data.frame() %>%
    .[.$Freq > 1, 1]

  if(length(dup_site) == 0) cat("No duplicate site names found. \n")
  if(length(dup_site) > 0) {
    tmp = qc_df %>%
      dplyr::filter(`Site Name` %in% dup_site) %>%
      dplyr::select(path_nm, GlobalID, `Site Name`) %>%
      dplyr::mutate(error_message = paste0(`Site Name`, " site name is a duplicate.")) %>%
      dplyr::select(- `Site Name`)

    cat("Duplicate site names found! Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, tmp)
  }

  #####
  # CHECK 3: Are there NAs in these columns?
  tmp = check_na(qc_df,
                 cols_to_check_nas)
  if( !is.null(tmp) ) qc_tmp = rbind(qc_tmp, tmp)

  #####
  # CHECK 4:  Is the latitude (y) or longitude (x) outside of expected values?
  cat("Checking whether lat/lon fall within expected values. \n")

  # set expected values
  lon_min = -125; lon_max = -110; lat_min = 40; lat_max = 50 # rough boundaries for Pacific Northwest

  # does x and y fall outside of expected values
  xy_chk = qc_df %>%
    dplyr::select(path_nm, GlobalID, x, y) %>%
    dplyr::rename(longitude = x,
                  latitude = y) %>%
    tidyr::pivot_longer(cols = longitude:latitude) %>%
    # TRUE = good, FALSE = outside expected values
    dplyr::mutate(
      chk = case_when(
        name == "longitude" ~ between(value, lon_min, lon_max),
        name == "latitude"  ~ between(value, lat_min, lat_max)
      )
    ) %>%
    dplyr::filter(chk == FALSE) %>%
    dplyr::mutate(
      error_message = case_when(
        name == "longitude" ~ paste0("The longitude (x) ", value, " falls outside the expected values ", lon_min, " and ", lon_max),
        name == "latitude"  ~ paste0("The latitude (y) ", value, " falls ouside the expected values ", lat_min, " and ", lat_max)
      )
    ) %>%
    dplyr::select(-name, -value, -chk)

  if( nrow(xy_chk) == 0 ) cat("All lat/lon fall within expected values. \n")
  if( nrow(xy_chk) > 0 ) {
    cat("Longitude and latitude values found outside of expected values. Adding to QC results. \n")
    qc_tmp = rbind(qc_tmp, xy_chk)
  }

  # return qc results
  return(qc_tmp)

} # end qc_survey()

