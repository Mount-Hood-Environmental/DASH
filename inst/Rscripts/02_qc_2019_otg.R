# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to QA/QC 2019 on-the-ground (OTG) DASH data from
# Survey 123. This is a follow-up to the script to read in the OTG data.
# My intent is to later convert this script into a series of functions.
#
# Created: August 12, 2020
#   Last Modified: September 3, 2020
#
# Notes:
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
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
# load the otg_data list of dfs
#-----------------------------
load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/prepped/raw_DASH_2019_otg.rda"))

#-----------------------------
# QC each otg_type, separately
#-----------------------------
# qc_surv_results = qc_survey(qc_df = otg_data$survey)         # surveyPoint_0.csv
# qc_cu_results = qc_cu(qc_df = otg_data$cu)                   # CU_1.csv
# qc_wood_results = qc_wood(qc_df = otg_data$wood)             # Wood_2.csv
# qc_jam_results = qc_jam(qc_df = otg_data$jam)                # Jam_3.csv
# qc_undercut_results = qc_undercut(qc_df = otg_data$undercut) # Undercut_4.csv
# qc_disch_results = qc_disch(qc_df = otg_data$discharge)      # Discharge_5.csv
# qc_disch_meas_results = qc_disch_meas(qc_df = otg_data$discharge_measurements)
#   # DischargeMeasurements_6.csv

#-----------------------------
# Combine all separate QC results
#-----------------------------
# qc_all = qc_surv_results %>%
#   tibble::add_column(source = "Survey",
#                      .before = 0) %>%
#   bind_rows(qc_cu_results %>%
#               tibble::add_column(source = "CU",
#                                  .before = 0)) %>%
#   bind_rows(qc_wood_results %>%
#               tibble::add_column(source = "Wood",
#                                  .before = 0)) %>%
#   bind_rows(qc_jam_results %>%
#               tibble::add_column(source = "Jam",
#                                  .before = 0)) %>%
#   bind_rows(qc_undercut_results %>%
#               tibble::add_column(source = "Undercut",
#                                  .before = 0)) %>%
#   bind_rows(qc_disch_results %>%
#               tibble::add_column(source = "Discharge",
#                                  .before = 0)) %>%
#   bind_rows(qc_disch_meas_results %>%
#               tibble::add_column(source = "DischargeMeasurements",
#                                  .before = 0))

#-----------------------------
# Using qc_wrapper() instead
#-----------------------------
# all QC options
qc_all = qc_wrapper(survey_df = otg_data$survey,
                    cu_df = otg_data$cu,
                    wood_df = otg_data$wood,
                    jam_df = otg_data$jam,
                    undercut_df = otg_data$undercut,
                    discharge_df = otg_data$discharge,
                    disch_meas_df = otg_data$discharge_measurements)

# just a couple, for example
# qc_some = qc_wrapper(survey_df = NULL,
#                     cu_df = NULL,
#                     wood_df = otg_data$wood,
#                     jam_df = otg_data$jam,
#                     undercut_df = NULL,
#                     discharge_df = NULL,
#                     disch_meas_df = NULL)

#-----------------------------
# Examine QC results
#-----------------------------
# where do all the QA/QC errors come from?
janitor::tabyl(qc_all, source)

# The vast majority of QC errors are in the CU data, and of those, most are related to ocular substrate estimates
# and fish cover estimates. Let's examine those further...

# Ocular substrate estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  select(path_nm, sum_cover) %>%
  #select(sum_cover) %>%
  table()

# We've created the rescale_values() and fix_fish_cover() argument to resolve many of those...

# First, ocular estimates, they should be a bit easier as they should sum to 100.
# ocular substrate columns
oc_cols = c("Sand/Fines 2mm",
            "Gravel 2-64mm",
            "Cobble 64-256mm",
            "Boulder 256mm")

# re-scale ocular estimate columns
otg_data$cu = rescale_values(data_df = otg_data$cu,
                             col_names = oc_cols,
                             min_value = 80,
                             max_value = 120,
                             sum_to = 100)

# Now, fish cover estimates...
# fish cover columns
cover_cols = c("Overhanging Cover",
               "Aquatic Vegetation",
               "Woody Debris Cover",
               "Artificial Cover",
               "Total No Cover")

# resolve fish cover issues using fix_fish_cover()
otg_data$cu = fix_fish_cover(cu_df = otg_data$cu,
                             cover_cols = cover_cols)

# Now, re-run the QC and write out those results
qc_all = qc_wrapper(survey_df = otg_data$survey,
                    cu_df = otg_data$cu,
                    wood_df = otg_data$wood,
                    jam_df = otg_data$jam,
                    undercut_df = otg_data$undercut,
                    discharge_df = otg_data$discharge,
                    disch_meas_df = otg_data$discharge_measurements)

# where do all the QA/QC errors come from?
janitor::tabyl(qc_all, source)

# Write out QC results
output_path = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/2019/lemhi/1_formatted_csvs/qc_results_DASH_2019_",
                     format(Sys.Date(), format = "%Y%m%d"),
                     ".csv")
readr::write_csv(qc_all, output_path)

# Now, let's examine some of the remaining QC flags
#####
# SURVEY
# latitude, longitude errors
qc_all %>%
  filter(source == "Survey") %>%
  left_join(otg_data$survey) %>%
  select(source:error_message, x:y)

#####
# CU
# `Maximum Depth (m)` errors
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:error_message, `Channel Unit Type`, `Maximum Depth (m)`) %>%
  #View()
  janitor::tabyl(`Channel Unit Type`)

# Duplicate channel units
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("appears more than once", error_message)) #%>%
  #View()

# Thalweg exit depth
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Thalweg exit depth", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:error_message,
         `Channel Unit Number`,
         `Channel Segment Number`,
         `Thalweg Exit Depth (m)`)

# Ocular substrate estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  #select(path_nm, sum_cover) %>%
  select(sum_cover) %>%
  table()

# Pebble counts
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Pebble size values", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(path_nm:GlobalID,
         contains("Pebble")) #%>%
  #View()

#####
# WOOD
qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Large Wood Number is <blank> or NA.", error_message)) %>%
  left_join(otg_data$wood)

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Diameter", error_message)) %>%
  left_join(otg_data$wood) %>%
  select(path_nm:GlobalID,
         ObjectID:`Ballasted?`) #%>%
  #View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  left_join(otg_data$wood) %>%
  select(path_nm:GlobalID,
         `Wet?`,
         `Channel Forming?`,
         `Ballasted?`) %>%
  pivot_longer(`Wet?`:`Ballasted?`) %>%
  filter(is.na(value)) #%>%
  #View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Length is less than or equal to the diameter|falls outside of the expected", error_message)) %>%
  left_join(otg_data$wood) %>%
  select(path_nm:GlobalID,
         `Length (m)`,
         `Diameter (m)`)

#####
# JAM
qc_all %>%
  filter(source == "Jam") %>%
  left_join(otg_data$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame() #%>%
  #View()

#####
# UNDERCUT
qc_all %>%
  filter(source == "Undercut") %>%
  left_join(otg_data$undercut) %>%
  select(path_nm:error_message,
         `Undercut Number`:`Width 75% (m)`) %>%
  as.data.frame() #%>%
  #View()

#####
# DISCHARGE MEASUREMENTS
qc_all %>%
  filter(source == "DischargeMeasurements") %>%
  left_join(otg_data$discharge_measurements) %>%
  select(path_nm:error_message,
         ObjectID:`Station Velocity`) %>%
  as.data.frame() #%>%
  #View()

######################################################################
# At this point, I fixed many errors identified by the QC 2020-09-02 #
######################################################################
# re-import QC'd data
path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/2_qcd_csvs/")
otg_qcd_data = read_otg_csv_wrapper(path = path,
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
beepr::beep(sound = "ping")

# Make our fixes to ocular substrate and fish cover estimates, which are a big chunk of the issues
otg_qcd_data$cu = rescale_values(data_df = otg_qcd_data$cu,
                                 col_names = c("Sand/Fines 2mm",
                                               "Gravel 2-64mm",
                                               "Cobble 64-256mm",
                                               "Boulder 256mm"),
                                 min_value = 80,
                                 max_value = 120,
                                 sum_to = 100)

otg_qcd_data$cu = fix_fish_cover(cu_df = otg_qcd_data$cu,
                                 cover_cols = c("Overhanging Cover",
                                                "Aquatic Vegetation",
                                                "Woody Debris Cover",
                                                "Artificial Cover",
                                                "Total No Cover"))


# perform QC on the qc'd data
qc_all = qc_wrapper(survey_df = otg_qcd_data$survey,
                    cu_df = otg_qcd_data$cu,
                    wood_df = otg_qcd_data$wood,
                    jam_df = otg_qcd_data$jam,
                    undercut_df = otg_qcd_data$undercut,
                    discharge_df = otg_qcd_data$discharge,
                    disch_meas_df = otg_qcd_data$discharge_measurements)

# Write out remaining QC results
output_path = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/2019/lemhi/2_qcd_csvs/qc_results_DASH_2019_",
                     format(Sys.Date(), format = "%Y%m%d"),
                     ".csv")
readr::write_csv(qc_all, output_path)

# save the qc'd data
save(otg_qcd_data,
     file = paste0(nas_prefix,"/data/habitat/DASH/OTG/2019/lemhi/prepped/qcd_DASH_2019_otg.rda"))

#-----------------------------
# compare the raw formatted data to the data where QC errors have been/will be addressed
#-----------------------------

# the directories to compare
path1 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/1_formatted_csvs/")
path2 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/old_2_qcd_csvs/")

# use compare_folders()
qc_changes = compare_folders(path1 = path1,
                             path2 = path2)
save(qc_changes,
     file = paste0(nas_prefix, "data/habitat/DASH/OTG/2019/lemhi/prepped/qc_resolutions.rda"))

### END SCRIPT




