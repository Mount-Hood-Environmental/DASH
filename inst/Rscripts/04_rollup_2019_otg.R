# Authors: Mike Ackerman
#
# Purpose: A script to clean and prep QC'd 2019 on-the-ground (OTG) DASH data.
# This script follows others to import and QC the raw data.
# Created: October 6, 2020
#   Lat Modified: October 6, 2020
#
# Notes:
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(DASH)

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
# load the otg_qcd list of dfs for lemhi and nf salmon
#-----------------------------
lemhi_otg = load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/prepped/qcd_DASH_2019_otg.rda")) %>%
  get()
nfsal_otg = load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/nf_salmon/prepped/qcd_DASH_2019_otg.rda")) %>%
  get()

# and merge them
otg = purrr::map2(lemhi_otg,
                  nfsal_otg,
                  dplyr::full_join)

# clean up
rm(lemhi_otg, nfsal_otg, otg_qcd)

# # write joined data
# output_path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/prepped/")
# save(otg,
#      file = paste0(output_path, "DASH_2019_otg.rda"))
#
# # and csvs (for now)
# write_csv(otg$survey, paste0(output_path, "survey.csv"))
# write_csv(otg$cu, paste0(output_path, "cu.csv"))
# write_csv(otg$wood, paste0(output_path, "wood.csv"))
# write_csv(otg$jam, paste0(output_path, "jam.csv"))
# write_csv(otg$undercut, paste0(output_path, "undercut.csv"))
# write_csv(otg$discharge, paste0(output_path, "discharge.csv"))
# write_csv(otg$discharge_measurements, paste0(output_path, "discharge_measurements.csv"))

#-----------------------------
# clean cu data.frame
#-----------------------------


#-----------------------------
# start rolling up data
#-----------------------------
# WOOD
cu_wood = rollup_cu_wood(wood_df = otg$wood)

# JAM
cu_jam = rollup_cu_jam(jam_df = otg$jam)

