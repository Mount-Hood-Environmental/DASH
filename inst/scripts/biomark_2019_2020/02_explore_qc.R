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
library(tidyverse)
library(magrittr)
library(janitor)
#library(DASH)
devtools::load_all()

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
# load all raw OTG data
#-------------------------

# list of otg_raw files in otg_path
otg_raw_files = list.files(path = otg_path,
                           pattern = "^otg_raw\\.rda$",
                           recursive = T) %>%
  paste0(otg_path, .)

# load the otg_raw files
lem_otg_raw = load(otg_raw_files[1]) %>% get()
nfs_otg_raw = load(otg_raw_files[2]) %>% get()
sec_otg_raw = load(otg_raw_files[3]) %>% get()

# join using map2
otg_raw_all = purrr::map2(lem_otg_raw,
                          nfs_otg_raw,
                          dplyr::full_join) %>%
  purrr::map2(sec_otg_raw,
              dplyr::full_join)

#-------------------------
# examine QC results
#-------------------------
# what data types do QC errors come from?
tabyl(qc_all,
      source)

# a majority are from the "CU" data, explore a little further...
qc_all %>%
  filter(source == "CU") %>%
  count(error_message) %>%
  print(n = 100)

### Survey ###
# latitude, longitude
qc_all %>%
  filter(source == "Survey") %>%
  left_join(otg_raw_all$survey) %>%
  select(source:error_message, x:y)

### CU ###
# `Maximum Depth (m)` errors
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
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
  left_join(otg_raw_all$cu) %>%
  select(source:error_message,
         `Channel Unit Number`,
         `Channel Segment Number`,
         `Thalweg Exit Depth (m)`)

# Ocular substrate estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
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
  left_join(otg_raw_all$cu) %>%
  select(path_nm:GlobalID,
         contains("Pebble")) #%>%
#View()

### Wood ###
qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Large Wood Number is <blank> or NA.", error_message)) %>%
  left_join(otg_raw_all$wood)

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Diameter", error_message)) %>%
  left_join(otg_raw_all$wood) %>%
  select(path_nm:GlobalID,
         ObjectID:`Ballasted?`) #%>%
#View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  left_join(otg_raw_all$wood) %>%
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
  left_join(otg_raw_all$wood) %>%
  select(path_nm:GlobalID,
         `Length (m)`,
         `Diameter (m)`)

### Jam ###
qc_all %>%
  filter(source == "Jam") %>%
  left_join(otg_raw_all$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame() #%>%
#View()

### Undercut ###
qc_all %>%
  filter(source == "Undercut") %>%
  left_join(otg_raw_all$undercut) %>%
  select(path_nm:error_message,
         `Undercut Number`:`Width 75% (m)`) %>%
  as.data.frame() #%>%
#View()

# Discharge measurements
qc_all %>%
  filter(source == "DischargeMeasurements") %>%
  left_join(otg_raw_all$discharge_measurements) %>%
  select(path_nm:error_message,
         ObjectID:`Station Velocity`) %>%
  as.data.frame() #%>%
#View()

# END SCRIPT
