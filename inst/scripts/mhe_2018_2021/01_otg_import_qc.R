# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to import and QC on-the-ground (OTG) DASH data from
# 2018 & 2021 using Survey123. This script will NOT work on the 2019 &
# 2020 OTG data, unfortunately, due to changes in the data collection
# forms.
#
# First Created: July 15, 2020
#   Last Modified: April 7, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(beepr)
library(janitor)
library(DASH)

#-----------------------------
# set some arguments/parameters
#-----------------------------
# set NAS prefix
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

# create a vector of directories containing the OTG data for 2018 & 2021 data (updated data collection forms)
yr_wtsd = c("2018/lemhi",
            "2018/pahsimeroi",
            "2018/upper_salmon",
            "2021/big_lost",
            "2021/mf_salmon",
            "2021/nf_salmon")

#-----------------------------
# LOOP 1: import raw OTG data; loop over year_watershed combinations
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd
  path = paste0(nas_prefix, "main/data/habitat/DASH/OTG/", yw, "/1_formatted_csvs/")

  # import OTG data
  otg_raw = read_otg_csv_wrapper(path = path)

  # QC each OTG type for each yr_wtsd
  otg_raw$qc_results = qc_wrapper(survey_df = otg_raw$survey,
                                  cu_df = otg_raw$cu,
                                  wood_df = otg_raw$wood,
                                  jam_df = otg_raw$jam,
                                  undercut_df = otg_raw$undercut,
                                  discharge_df = otg_raw$discharge,
                                  channel_unit_roll_qc = T)

  # save the otg_raw list of dfs, and the initial QC flags
  save(otg_raw,
       file = paste0(nas_prefix, "main/data/habitat/DASH/OTG/", yw, "/1_formatted_csvs/otg_raw.rda"))

  rm(otg_raw, path)

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import raw, QC, and save loop

#-----------------------------
# LOOP 2: iterative loop: import "QC'd" .csvs, run QC, write QC results,
# fix problems in said "QC'd" .csvs, rinse and repeat...
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd
  path = paste0(nas_prefix, "main/data/habitat/DASH/OTG/", yw, "/2_qcd_csvs/")

  # import OTG data
  otg_interim = read_otg_csv_wrapper(path = path)

  # perform QC on the qc'd data
  qc_interim = qc_wrapper(survey_df = otg_interim$survey,
                          cu_df = otg_interim$cu,
                          wood_df = otg_interim$wood,
                          jam_df = otg_interim$jam,
                          undercut_df = otg_interim$undercut,
                          discharge_df = otg_interim$discharge,
                          channel_unit_roll_qc = T)

  # write interim QC results
  write_csv(qc_interim, paste0(path, "qc_interim_", format(Sys.Date(), format = "%Y%m%d"), ".csv"))

  # save the otg_interim list of dfs
  save(otg_interim,
       file = paste0(path, "otg_interim.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end interim QC loop

# IMPORTANT: THE ABOVE LOOP 2) IMPORTING QC'D DATA AND QC'ING THAT DATA IS A
# POTENTIALLY ITERATIVE PROCESS, DESCRIBED BELOW

# At this point, someone very familiar with the OTG data, (preferably a field technician or field coordinator,
# secondarily a project leader) should intervene, review the QC errors flagged on the /2_qcd_csvs/ versions of
# the data that we just wrote out, attempt to resolve those, and ideally, make notes for those QC errors that can't
# be resolved. Errors should be resolved to the .csvs within the survey folders in the /2_qcd_csvs/ directory.
#
# After resolving QC's to the best of your abilities, the next step is to re-run the above loop. This will
# re-import the .csvs with the "improved" data and re-run the QC, exporting a new summary of remaining flags,
# which can then be reviewed again. This process can be repeated until as many errors are resolved as possible.
# It is fine to add notes in the above "qc_interim_YYYYMMDD.csv" data about how errors were or were not resolved.

#-----------------------------
# LOOP 3: re-import "QC" .csvs, perform final QC
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd
  path = paste0(nas_prefix, "main/data/habitat/DASH/OTG/", yw, "/2_qcd_csvs/")

  # import OTG data
  otg_qcd = read_otg_csv_wrapper(path = path)

  # perform QC on the "final" OTG data
  qc_final = qc_wrapper(survey_df = otg_qcd$survey,
                        cu_df = otg_qcd$cu,
                        wood_df = otg_qcd$wood,
                        jam_df = otg_qcd$jam,
                        undercut_df = otg_qcd$undercut,
                        discharge_df = otg_qcd$discharge,
                        channel_unit_roll_qc = T)

  # write "final" QC results
  otg_qcd$qc_results = qc_final

  # save as .Rdata object
  save(otg_qcd, file = paste0(nas_prefix, "main/data/habitat/DASH/OTG/", yw, "/prepped/otg_qcd.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import QC'd data, record final QC messages, and export prepped data

# END SCRIPT
