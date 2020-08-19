# Authors: Mike Ackerman
#
# Purpose: A script to import all of the 2019 on-the-ground (OTG) DASH data from
# Survey 123. My intent is to later convert this script into a series of functions.
#
# Created: July 15, 2020
#   Last Modified:
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------
library(magrittr)

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
# set some arguments/parameters
#-----------------------------
path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/QA_QCd_csvs/")

#-----------------------------
# read in one type of OTG data
#-----------------------------

# read CU data; as an example
cu_df = read_otg_csv(path,
                     otg_type = "CU_1.csv")

#-----------------------------
# loop over OTG data types using wrapper function
#-----------------------------
otg_data = read_otg_csv_wrapper(path = path,
                                otg_type_list = c("surveyPoint_0.csv",
                                                  "CU_1.csv",
                                                  "Wood_2.csv",
                                                  "Jam_3.csv",
                                                  "Undercut_4.csv",
                                                  "Discharge_5.csv",
                                                  "DischargeMeasurements_6.csv"),
                                otg_type_names = c("survey",
                                                   "cu",
                                                   "wood",
                                                   "jam",
                                                   "undercut",
                                                   "discharge",
                                                   "discharge_measurements"))

#-----------------------------
# save the otg_data list of dfs
#-----------------------------
save(otg_data,
     file = paste0(nas_prefix,"/data/habitat/DASH/OTG/2019/lemhi/prepped/raw_DASH_2019_otg.rda"))

#-----------------------------
# testing compare two files
#-----------------------------

# the directories to compare
path1 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/raw/")
path2 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/QA_QCd_csvs/")

# use compare_folders()
compare_results = compare_folders(path1 = path1,
                                  path2 = path2)

### END SCRIPT
