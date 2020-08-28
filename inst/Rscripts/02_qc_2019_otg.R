# Authors: Mike Ackerman
#
# Purpose: A script to QA/QC 2019 on-the-ground (OTG) DASH data from
# Survey 123. This is a follow-up to the script to read in the OTG data.
# My intent is to later convert this script into a series of functions.
#
# Created: August 12, 2020
#   Last Modified: August 28, 2020
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------
library(dplyr)

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

# delete some rows with all missing values
otg_data$jam <- dplyr::filter(otg_data$jam,
                              !is.na(ObjectID))

#-----------------------------
# QA/QC surveyPoint_0.csv
#-----------------------------
qc_surv_results = qc_survey(qc_df = otg_data$survey,
                            otg_type = "surveyPoint_0.csv",
                            cols_to_check_nas = c("Survey Date",
                                                  "Survey Time"))

#-----------------------------
# QA/QC CU_1.csv
#-----------------------------
qc_cu_results = qc_cu(qc_df = otg_data$cu)

#-----------------------------
# QA/QC Wood_2.csv
#-----------------------------
qc_wood_results = qc_wood(qc_df = otg_data$wood)

#-----------------------------
# QA/QC Jam_3.csv
#-----------------------------
qc_jam_results = qc_jam(qc_df = otg_data$jam)

#-----------------------------
# QA/QC Undercut_4.csv
#-----------------------------
qc_undercut_results = qc_undercut(qc_df = otg_data$undercut)

#-----------------------------
# QA/QC Discharge_5.csv
#-----------------------------
qc_disch_results = qc_disch(qc_df = otg_data$discharge)

#-----------------------------
# QA/QC DischargeMeasurements_6.csv
#-----------------------------
qc_disch_meas_results = qc_disch_meas(qc_df = otg_data$discharge_measurements)


#-----------------------------
# Combine all QA/QC results
#-----------------------------
qc_all = qc_surv_results %>%
  tibble::add_column(Source = "Survey",
                     .before = 0) %>%
  bind_rows(qc_cu_results %>%
              tibble::add_column(Source = "CU",
                                 .before = 0)) %>%
  bind_rows(qc_wood_results %>%
              tibble::add_column(Source = "Wood",
                                 .before = 0)) %>%
  bind_rows(qc_jam_results %>%
              tibble::add_column(Source = "Jam",
                                 .before = 0)) %>%
  bind_rows(qc_undercut_results %>%
              tibble::add_column(Source = "Undercut",
                                 .before = 0)) %>%
  bind_rows(qc_disch_results %>%
              tibble::add_column(Source = "Discharge",
                                 .before = 0)) %>%
  bind_rows(qc_disch_meas_results %>%
              tibble::add_column(Source = "DischargeMeasurements",
                                 .before = 0))


# where do all the QA/QC errors come from?
janitor::tabyl(qc_all, Source)

# one way to examine the QA/QC results
qc_all %>%
  filter(Source == "Jam") %>%
  left_join(otg_data$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame()
