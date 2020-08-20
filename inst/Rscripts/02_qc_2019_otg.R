# Authors: Mike Ackerman
#
# Purpose: A script to QA/QC 2019 on-the-ground (OTG) DASH data from
# Survey 123. This is a follow-up to the script to read in the OTG data.
# My intent is to later convert this script into a series of functions.
#
# Created: August 12, 2020
#   Last Modified:
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------
library(tibble)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-----------------------------
# load the otg_data list of dfs
#-----------------------------
load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/prepped/raw_DASH_2019_otg.rda"))

#-----------------------------
# QA/QC surveyPoint_0.csv
#-----------------------------
tmp = otg_data$survey
qaqc_tmp = qaqc_tbl()

# CHECK 1: Are there duplicate site names?
dup_site = tmp$`Site Name` %>%
  table() %>%
  data.frame() %>%
  .[.$Freq > 1, 1]

tst = c(tmp$`Site Name`, "Arbon_2019", "Canyon_2019")
dup_site = tst %>%
  table() %>%
  data.frame() %>%
  .[.$Freq > 1, 1]

if(length(dup_site) == 0) {
  cat("No duplicate site names found in survey data.")
}

if(length(dup_site) > 0) {
  chk1 = tmp %>%
    dplyr::filter(`Site Name` %in% dup_site) %>%
    dplyr::select(path_nm, GlobalID, `Site Name`) %>%
    dplyr::mutate(error_message = paste0(`Site Name`, " site name is a duplicate.")) %>%
    dplyr::select(- `Site Name`)

  qaqc_tmp = rbind(qaqc_tmp, chk1)
}

# CHECK 2: Are these columns blank?
cols = c("Survey Time", "Survey Date")
chk2 = tmp %>%
  dplyr::select(path_nm, GlobalID, all_of(cols)) %>%
  dplyr::filter_at(., vars(cols), any_vars(is.na(.))) %>%
  tidyr::gather(key = "col_name", "value", all_of(cols)) %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::mutate(error_message = paste0("Column ", col_name, " is <blank> or NA.")) %>%
  dplyr::select(-col_name, -value)



# CHECK 2: Is survey date blank?
chk2 = tmp %>%
  #dplyr::select(path_nm, GlobalID, `Survey Date`) %>%
  dplyr::select(path_nm, GlobalID, all_of(col)) %>%
  dplyr::filter(is.na(`Survey Date`)) %>%
  dplyr::mutate(error_message = "Survey Date column is <blank> or NA") %>%
  dplyr::select(- `Survey Date`)

if(nrow(chk2) > 0) { qaqc_tmp = rbind(qaqc_tmp, chk2) }

# CHECK 3: Is survey time blank?
chk3 = tmp %>%
  #dplyr::select(path_nm, GlobalID, `Survey Time`) %>%
  dplyr::select(path_nm, GlobalID, all_of(col)) %>%
  dplyr::filter(is.na(`Survey Time`)) %>%
  dplyr::mutate(error_message = "Survey Time column is <blank> or NA") %>%
  dplyr::select(- `Survey Time`)

if(nrow(chk3) > 0) { qaqc_tmp = rbind(qaqc_tmp, chk3) }

col = "Survey Date"

# CHECK 4: Is the latitude (y) or longitude (x) missing?


