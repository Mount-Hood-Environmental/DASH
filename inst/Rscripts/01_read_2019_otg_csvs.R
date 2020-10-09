# Authors: Mike Ackerman
#
# Purpose: A script to import all of the 2019 on-the-ground (OTG) DASH data from
# Survey 123.
#
# Created: July 15, 2020
#   Last Modified: October 2, 2020
#
# Notes:
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(beepr)

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
#trib = "lemhi"
trib = "nf_salmon"

path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/1_formatted_csvs/")

#-----------------------------
# read in one type of OTG data
#-----------------------------
# read survey data; as an example
# cu_df = read_otg_csv(path,
#                      otg_type = "CU_1.csv")

#-----------------------------
# loop over OTG data types using the wrapper function
#-----------------------------
otg_raw = read_otg_csv_wrapper(path = path,
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
beepr::beep(3)

#-----------------------------
# save the otg_data list of dfs
#-----------------------------
save(otg_raw,
     file = paste0(nas_prefix,"/data/habitat/DASH/OTG/2019/", trib, "/prepped/raw_DASH_2019_otg.rda"))

#-----------------------------
# compare the raw *unformatted* data to the raw *formatted* data
#-----------------------------

# the directories to compare
path1 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/0_raw_csvs/")
path2 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/1_formatted_csvs/")

# use compare_folders()
compare_results = compare_folders(path1 = path1,
                                  path2 = path2)

### END SCRIPT
