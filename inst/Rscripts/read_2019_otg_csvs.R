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
library(purrr)

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
path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/")

#-----------------------------
# read in OTG data
#-----------------------------
# read surveyPoint data
survey_df = read_otg_csv(path,
                         otg_type = "surveyPoint_0.csv")

# read CU data
cu_df = read_otg_csv(path,
                     otg_type = "CU_1.csv")

# read Wood data
wood_df = read_otg_csv(path,
                       otg_type = "Wood_2.csv")

# read Jam data
jam_df = read_otg_csv(path,
                      otg_type = "Jam_3.csv")

# read Undercut data
undercut_df = read_otg_csv(path,
                           otg_type = "Undercut_4.csv")

# read Discharge data
discharge_df = read_otg_csv(path,
                            otg_type = "Discharge_5.csv")

# read DischargeMeasurements data
discharge_measurements_df = read_otg_csv(path,
                                         otg_type = "DischargeMeasurements_6.csv")


