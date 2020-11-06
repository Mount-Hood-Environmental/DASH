# Authors: Mike Ackerman
#
# Purpose: A script to explore QC results that are written out
# in the 01_otg_import_qc.R script
#
# Created: November 6, 2020
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
#library(tidyverse)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-------------------------
# load all QC results
#-------------------------
otg_path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/")

qc_all = list.files(path = otg_path,
                    #pattern = "\\qc_results.rds$",
                    pattern = "^qc_results.*\\.csv$",
                    recursive = T) %>%
  paste0(otg_path, .) %>%
  map_df(read_csv)

#-------------------------
# examine QC results
#-------------------------


########################################################################
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

# END SCRIPT
