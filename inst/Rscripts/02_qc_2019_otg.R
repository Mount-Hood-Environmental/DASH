# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to QA/QC 2019 on-the-ground (OTG) DASH data from
# Survey 123. This is a follow-up to the script to read in the OTG data.
# My intent is to later convert this script into a series of functions.
#
# Created: August 12, 2020
#   Last Modified: September 1, 2020
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------
library(dplyr)
library(readr)
library(janitor)

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
qc_surv_results = qc_survey(qc_df = otg_data$survey)         # surveyPoint_0.csv
qc_cu_results = qc_cu(qc_df = otg_data$cu)                   # CU_1.csv
qc_wood_results = qc_wood(qc_df = otg_data$wood)             # Wood_2.csv
qc_jam_results = qc_jam(qc_df = otg_data$jam)                # Jam_3.csv
qc_undercut_results = qc_undercut(qc_df = otg_data$undercut) # Undercut_4.csv
qc_disch_results = qc_disch(qc_df = otg_data$discharge)      # Discharge_5.csv
qc_disch_meas_results = qc_disch_meas(qc_df = otg_data$discharge_measurements)
  # DischargeMeasurements_6.csv

#-----------------------------
# Combine all separate QC results
#-----------------------------
qc_all = qc_surv_results %>%
  tibble::add_column(Source = "Survey",
                     .before = 0) %>%
  bind_rows(qc_cu_results %>%
              tibble::add_column(Source = "CU",
                                 .before = 0)) %>%
  bind_rows(qc_wood_results %>%
              tibble::add_column(Source = "Wood",
                                 .before = 0)) %>%
  bind_rows(qc_jam_results %>%
              tibble::add_column(Source = "Jam",
                                 .before = 0)) %>%
  bind_rows(qc_undercut_results %>%
              tibble::add_column(Source = "Undercut",
                                 .before = 0)) %>%
  bind_rows(qc_disch_results %>%
              tibble::add_column(Source = "Discharge",
                                 .before = 0)) %>%
  bind_rows(qc_disch_meas_results %>%
              tibble::add_column(Source = "DischargeMeasurements",
                                 .before = 0))

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
qc_all = qc_wrapper(survey_df = NULL,
                    cu_df = NULL,
                    wood_df = otg_data$wood,
                    jam_df = otg_data$jam,
                    undercut_df = NULL,
                    discharge_df = NULL,
                    disch_meas_df = NULL)

# Write out QC results
output_path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/1_formatted_csvs/qc_results_DASH_2019.csv")
readr::write_csv(qc_all, output_path)

#-----------------------------
# Examine QC results
#-----------------------------
# where do all the QA/QC errors come from?
janitor::tabyl(qc_all, source)

#####
# SURVEY

# latitude, longitude errors
qc_all %>%
  filter(source == "Survey") %>%
  left_join(otg_data$survey) %>%
  select(source:error_message, x:y) %>%
  View()

#####
# CU

# `Maximum Depth (m)` errors
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  left_join(otg_data$cu) %>%
  select(source:error_message, `Channel Unit Type`, `Maximum Depth (m)`) %>%
  View()
  janitor::tabyl(`Channel Unit Type`)

# Duplicate channel units
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("appears more than once", error_message)) %>%
  View()

# Continue Here

#####
# WOOD

#####
# JAM
# one way to examine the QA/QC results
qc_all %>%
  filter(source == "Jam") %>%
  left_join(otg_data$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame()

#####
# UNDERCUT

#####
# DISCHARGE

#####
# DISCHARGE MEASUREMENTS
