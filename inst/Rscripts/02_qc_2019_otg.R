# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to QA/QC 2019 on-the-ground (OTG) DASH data from
# Survey 123. This is a follow-up to the script to read in the OTG data.
# We QC the raw data, resolve some issues identified, flag the remaining
# and output a .csv of those, and re-write the data.
#
# Created: August 12, 2020
#   Last Modified: October 2, 2020
#
# Notes:
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)

#library(readr)
#library(tidyverse)
#library(janitor)
#library(beepr)

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
# As an example, QC just a couple otg_types, separately
#-----------------------------
qc_cu_results = qc_cu(qc_df = otg_raw$cu)
qc_wood_results = qc_wood(qc_df = otg_raw$wood)

# combine those together
qc_some = qc_cu_results %>%
  tibble::add_column(source = "CU",
                     .before = 0) %>%
  bind_rows(qc_wood_results %>%
              tibble::add_column(source = "Wood",
                                 .before = 0))

# or using the qc_wrapper function instead
qc_some2 = qc_wrapper(survey_df = NULL,
                      cu_df = otg_raw$cu,
                      wood_df = otg_raw$wood,
                      jam_df = NULL,
                      undercut_df = NULL,
                      discharge_df = NULL,
                      disch_meas_df = NULL)

#-----------------------------
# For real, let's QC all the raw data
#-----------------------------
qc_raw = qc_wrapper(survey_df = otg_raw$survey,
                    cu_df = otg_raw$cu,
                    wood_df = otg_raw$wood,
                    jam_df = otg_raw$jam,
                    undercut_df = otg_raw$undercut,
                    discharge_df = otg_raw$discharge,
                    disch_meas_df = otg_raw$discharge_measurements)

#-----------------------------
# Quick look at the initial QC results
#-----------------------------
# where do all the QA/QC errors come from?
janitor::tabyl(qc_raw, source)

# a large chunk are from the "CU" data, examine a little further...
qc_raw %>%
  filter(source == "CU") %>%
  count(error_message) %>%
  print(n = 100)

# Note many errors in the "CU" data are related to ocular substrate estimates not summing to 100
# and fish cover estimates not summing to within an expected range. Let's resolve those using
# some functions we've created within the DASH package

# Ocular substrate estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# couple ocular substrate estimates that sum to 90, resolve using rescale_values()
otg_raw$cu = rescale_values(data_df = otg_raw$cu,
                            col_names = c("Sand/Fines 2mm",
                                          "Gravel 2-64mm",
                                          "Cobble 64-256mm",
                                          "Boulder 256mm"),
                            min_value = 90,
                            max_value = 110,
                            sum_to = 100)

# Fish cover estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  select(path_nm, sum_cover) %>%
  #select(sum_cover) %>%
  table()

# lots of issues with fish cover estimates, resolve some using fix_fish_cover()...
otg_raw$cu = fix_fish_cover(cu_df = otg_raw$cu,
                            cover_cols = c("Overhanging Cover",
                                           "Aquatic Vegetation",
                                           "Woody Debris Cover",
                                           "Artificial Cover",
                                           "Total No Cover"))

#-----------------------------
# Re-run the QC after resolving some ocular and cover issues
#-----------------------------
qc_raw = qc_wrapper(survey_df = otg_raw$survey,
                    cu_df = otg_raw$cu,
                    wood_df = otg_raw$wood,
                    jam_df = otg_raw$jam,
                    undercut_df = otg_raw$undercut,
                    discharge_df = otg_raw$discharge,
                    disch_meas_df = otg_raw$discharge_measurements)

#-----------------------------
# Examine the remaining QC errors in R
#-----------------------------
# latitude, longitude errors
qc_raw %>%
  filter(source == "Survey") %>%
  left_join(otg_raw$survey) %>%
  select(source:error_message, x:y)

# `Maximum Depth (m)` errors
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:error_message, `Channel Unit Type`, `Maximum Depth (m)`) %>%
  #View()
  janitor::tabyl(`Channel Unit Type`)

# Duplicate channel units
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("appears more than once", error_message)) #%>%
  #View()

# Thalweg exit depth
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Thalweg exit depth", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:error_message,
         `Channel Unit Number`,
         `Channel Segment Number`,
         `Thalweg Exit Depth (m)`)

# Ocular substrate estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  #select(path_nm, sum_cover) %>%
  select(sum_cover) %>%
  table()

# Pebble counts
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Pebble size values", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(path_nm:GlobalID,
         contains("Pebble")) #%>%
  #View()

# Wood
qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Large Wood Number is <blank> or NA.", error_message)) %>%
  left_join(otg_raw$wood)

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Diameter", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         ObjectID:`Ballasted?`) #%>%
  #View()

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         `Wet?`,
         `Channel Forming?`,
         `Ballasted?`) %>%
  pivot_longer(`Wet?`:`Ballasted?`) %>%
  filter(is.na(value)) #%>%
  #View()

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Length is less than or equal to the diameter|falls outside of the expected", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         `Length (m)`,
         `Diameter (m)`)

# Jam
qc_raw %>%
  filter(source == "Jam") %>%
  left_join(otg_raw$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame() #%>%
  #View()

# Undercut
qc_raw %>%
  filter(source == "Undercut") %>%
  left_join(otg_raw$undercut) %>%
  select(path_nm:error_message,
         `Undercut Number`:`Width 75% (m)`) %>%
  as.data.frame() #%>%
  #View()

# Discharge measurements
qc_raw %>%
  filter(source == "DischargeMeasurements") %>%
  left_join(otg_raw$discharge_measurements) %>%
  select(path_nm:error_message,
         ObjectID:`Station Velocity`) %>%
  as.data.frame() #%>%
  #View()

# write out remaining QC error messages
output_path = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/2019/lemhi/1_formatted_csvs/qc_results_DASH_2019_",
                     format(Sys.Date(), format = "%Y%m%d"),
                     ".csv")
readr::write_csv(qc_raw, output_path)

# At this point, someone very familiar with the OTG data (field technician, field coordinator)
# should likely intervene, review the remaining QC errors that we just wrote out, attempt to resolve those,
# and ideally make notes for those that can't be resolved, and for those than can, identify how
# the issue was resolved. Changes to any data should be made to a copy of all of the data i.e.,
# do not modify the raw data!

# END SCRIPT
