# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to import and QC all of the 2018 & 2021 on-the-ground (OTG)
# DASH data collected using Survey123. This script will NOT work on the 2019 &
# 2020 OTG data, which was collected using old data collection forms.
#
# Created: July 15, 2020
#   Last Modified: February 28, 2022
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

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-----------------------------
# set some arguments/parameters
#-----------------------------
# create a vector to directories containing the raw data, formatted for import
# 2018 & 2021 data (updated data collection forms)
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
  path = paste0(nas_prefix,
                "Public Data/data/habitat/DASH/OTG/",
                yw,
                "/1_formatted_csvs/")

  # loop over OTG data types using the wrapper function
  otg_raw = read_otg_csv_wrapper(path = path,
                                 otg_type_list = c("surveyPoint_0.csv",
                                                   "CU_1.csv",
                                                   "Wood_2.csv",
                                                   "Jam_3.csv",
                                                   "Undercut_4.csv",
                                                   "Discharge_5.csv"),
                                 otg_type_names = c("survey",
                                                    "cu",
                                                    "wood",
                                                    "jam",
                                                    "undercut",
                                                    "discharge"))

  # QC all data types for each yr_wtsd
  otg_raw$qc_results = qc_wrapper(survey_df = otg_raw$survey,
                                  cu_df = otg_raw$cu,
                                  wood_df = otg_raw$wood,
                                  jam_df = otg_raw$jam,
                                  undercut_df = otg_raw$undercut,
                                  discharge_df = otg_raw$discharge,
                                  channel_unit_roll_qc = TRUE)

  # save the otg_raw list of dfs, and the initial QC flags
  save(otg_raw,
       file = paste0(nas_prefix,
                     "Public Data/data/habitat/DASH/OTG/",
                     yw,
                     "/1_formatted_csvs/otg_raw.rda"))

  rm(otg_raw, path)

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import raw, QC, and save loop



#-----------------------------
# LOOP 2: iterative loop: import "QC" .csvs, run QC, write QC results,
# fix problems in said "QC" .csvs, rinse and repeat...
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd, except QC'd data
  path = paste0(nas_prefix,
                "/data/habitat/DASH/OTG/",
                yw,
                "/2_qcd_csvs/")

  otg_interim = read_otg_csv_wrapper(path = path,
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

  # perform QC on the qc'd data
  qc_interim = qc_wrapper(survey_df = otg_interim$survey,
                          cu_df = otg_interim$cu,
                          wood_df = otg_interim$wood,
                          jam_df = otg_interim$jam,
                          undercut_df = otg_interim$undercut,
                          discharge_df = otg_interim$discharge,
                          discharge_meas_df = otg_interim$discharge_measurements,
                          channel_unit_roll_qc = T)

  # write interim QC results
  write_csv(qc_interim, paste0(path,
                               "qc_interim_",
                               format(Sys.Date(), format = "%Y%m%d"),
                               ".csv"))

  # save the otg_interim list of dfs
  save(otg_interim,
       file = paste0(path,
                     "otg_interim.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end interim QC loop

# IMPORTANT: THE ABOVE LOOP 1) IMPORTING QC'D DATA AND QC'ING THAT DATA IS A
# POTENTIALLY ITERATIVE PROCESS, DESCRIBED BELOW

# At this point, someone very familiar with the OTG data, (preferably a field technician or field coordinator,
# secondarily a project leader) should intervene, review the QC errors flagged on the /2_qcd_csvs/ versions of
# the data that we just wrote out, attempt to resolve those, and ideally, make notes for those QC errors that can't
# be resolved. Errors should be resolved to the .csvs within the survey folders in the /2_qcd_csvs/ directory.

# In addition, for the QC errors that are resolved, it is useful to provide notes on how they are resolved. Those
# notes on how errors were/were not resolved care helpful towards improving potential data validation (i.e.,
# validation of data during field surveys) or quality control steps in the future.

# After resolving QC's to the best of your abilities, the next step is to re-run the above loop. This will
# re-import the .csvs with the "improved" data and re-run the QC, exporting a new summary of remaining flags,
# which can then be reviewed again. This process can be repeated until as many errors are resolved as possible.
# It is fine to add notes in the above "qc_final_YYYYMMDD.csv" data about how errors were or were not resolved.

#-----------------------------
# LOOP 3: re-import "QC" .csvs, perform final QC
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd, except QC'd data
  path = paste0(nas_prefix,
                "/data/habitat/DASH/OTG/",
                yw,
                "/2_qcd_csvs/")

  otg_qcd = read_otg_csv_wrapper(path = path,
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

  # perform QC on the "final" OTG data
  qc_final = qc_wrapper(survey_df = otg_qcd$survey,
                        cu_df = otg_qcd$cu,
                        wood_df = otg_qcd$wood,
                        jam_df = otg_qcd$jam,
                        undercut_df = otg_qcd$undercut,
                        discharge_df = otg_qcd$discharge,
                        discharge_meas_df = otg_qcd$discharge_measurements,
                        channel_unit_roll_qc = T)

  # write "final" QC results
  otg_qcd$qc_results = qc_final

  # save as .Rdata object
  save(otg_qcd,
       file = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/",
                     yw,
                     "/prepped/otg_qcd.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import QC'd data, record final QC messages, and export prepped data

# END SCRIPT
