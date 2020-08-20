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
qc_surv_results = qc_survey(qc_df = otg_data$survey,
                            otg_type = "surveyPoint_0.csv",
                            cols_to_check_nas = c("Survey Date",
                                                  "Survey Time"))

# CHECK 4: Is the latitude (y) or longitude (x) missing?


