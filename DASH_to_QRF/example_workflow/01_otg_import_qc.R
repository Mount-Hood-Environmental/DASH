# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to import and QC on-the-ground (OTG) DASH data from
# 2022 using Survey123. This script will NOT work on the 2019 &
# 2020 OTG data, unfortunately, due to changes in the data collection
# forms.
#
# First Created: July 15, 2020
# Last Modified: April 24, 2024 by Bridger Bertram

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------

library(tidyverse)
library(beepr)
library(janitor)
library(DASH)
library(here)

#-----------------------------
# set some arguments/parameters
#-----------------------------

# create a vector of directories containing the OTG data. For this template,
# we will use the example_data directory. replace this with project_data directory

# Example directory
data_directory = here("data/example_data")

# Project directory
#data_directory = here("data/project_data")

#-----------------------------
# LOOP 1: import raw OTG data; loop over year_watershed combinations.
#
# NOTE: Save Raw csvs form DASH survey "0_raw_csvs" and save data with formatted csv names to "1_formatted_csvs".
#       Folders should be saved within the project_data directory.
#       We will preform an initial QC on data saved in "1_formatted_csvs" and leave "0_raw_csvs"
#       as raw data. The RDA file saved in 1_formatted_csvs will contain the initial QC flags.
#
# Folder Names within project_data/1_formatted_csvs/
#   surveyPoint_0
#   CU_1
#   Wood_2
#   Jam_3
#   Undercut_4
#   Discharge_5
#-----------------------------
for (data in data_directory) {

  # set path for yr_wtsd
  path = paste0(data_directory, "/1_formatted_csvs/")
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
       file = paste0(data_directory, "/1_formatted_csvs/otg_raw.rda"))

  #rm(otg_raw, path)

} # end import raw, QC, and save loop

#-----------------------------
# LOOP 2: iterative loop: import "QC'd" .csvs, run QC, write QC results,
#         fix problems in said "QC'd" .csvs, rinse and repeat...
#
# NOTE : For this loop, Make a copy of otg data from "1_formatted_csvs" and save
#        it into a new folder called "2_qcd_csvs". Each iteration of this loop saves a
#        csv file describing the qc flags. This is the step where you can manually edit the
#        csv files as needed.
#-----------------------------

for (data in data_directory) {

  # set path for yr_wtsd
  path = paste0(data_directory, "/2_qcd_csvs/")

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
#
# Note: This loop will pull the csvs from "2_qcd_csvs" folder, and preform final QC
#-----------------------------
for (data in data_directory) {

  # set path for yr_wtsd
  path = paste0(data_directory, "/2_qcd_csvs/")

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

} # end import QC'd data, record final QC messages, and export prepped data. Make sure to create "3_prepped_otg" folder before saving.

save_final_otg_to_this_path <- paste0(data_directory,"/3_prepped_otg/otg_qcd.rds")
saveRDS(otg_qcd, save_final_otg_to_this_path)

# END SCRIPT


