# Authors: Mike Ackerman
#
# Purpose: A script to clean and prep QC'd 2019 on-the-ground (OTG) DASH data.
# This is a follow-up to the scripts to read in and QC the OTG data.
# My intent is to later convert this script into a series of functions.
#
# Created: September 3, 2020
#   Last Modified:
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------

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
# load the otg_qcd_data list of dfs
#-----------------------------
load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/prepped/qcd_DASH_2019_otg.rda"))
