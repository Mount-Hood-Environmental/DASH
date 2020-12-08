# Authors: Mike Ackerman
#
# Purpose: A script to explore QC results that are written out
# in the 01_otg_import_qc.R script
#
# Created: November 6, 2020
#   Last Modified: November 18, 2020
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
library(DASH)
# devtools::load_all()

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
  as.list() %>%
  map_df(read_csv) %>%
  clean_names()

#-------------------------
# load all raw OTG data
#-------------------------

# list of otg_raw files in otg_path
otg_raw_files = list.files(path = otg_path,
                           pattern = "^otg_raw\\.rda$",
                           recursive = T) %>%
  paste0(otg_path, .)

# load the otg_raw files, and join them together
otg_list = as.list(otg_raw_files) %>%
  map(.f = function(x) {
    load(x) %>%
      get() %>%
      map(clean_names)
  })

for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg_raw_all = otg_list[[1]]
  } else {
    otg_raw_all = suppressMessages(purrr::map2(otg_raw_all,
                                               otg_list[[i]],
                                               dplyr::full_join))
  }
}

# clean up
rm(otg_raw_files,
   otg_list)



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
# latitude, longitude flags
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
  select(source:error_message, channel_unit_type, maximum_depth_m) %>%
  # View()
  janitor::tabyl(channel_unit_type)

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
         channel_unit_number,
         channel_segment_number,
         thalweg_exit_depth_m)

# Ocular substrate estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
  select(source:global_id,
         channel_unit_type,
         sand_fines_2mm:boulder_256mm) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
  select(source:global_id,
         channel_unit_type,
         overhanging_cover:total_no_cover) %>%
  mutate(sum_cover = rowSums(.[5:9], na.rm = T)) %>%
  #select(path_nm, sum_cover) %>%
  tabyl(sum_cover)

# Pebble counts
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Pebble size values", error_message)) %>%
  left_join(otg_raw_all$cu) %>%
  select(path_nm:global_id,
         contains("pebble")) #%>%
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
  select(path_nm:global_id,
         object_id:ballasted) #%>%
#View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  left_join(otg_raw_all$wood) %>%
  select(path_nm:global_id,
         object_id:ballasted) %>%
  pivot_longer(wet:ballasted) %>%
  filter(is.na(value)) #%>%
#View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Length is less than or equal to the diameter|falls outside of the expected", error_message)) %>%
  left_join(otg_raw_all$wood) %>%
  select(path_nm:global_id,
         length_m,
         diameter_m)

### Jam ###
qc_all %>%
  filter(source == "Jam") %>%
  left_join(otg_raw_all$jam) %>%
  select(-(parent_global_id:editor)) %>%
  as.data.frame() #%>%
#View()

### Undercut ###
qc_all %>%
  filter(source == "Undercut") %>%
  left_join(otg_raw_all$undercut) %>%
  select(path_nm:error_message,
         undercut_number:width_75_percent_m) %>%
  as.data.frame() #%>%
# View()

# Discharge measurements
qc_all %>%
  filter(source == "DischargeMeasurements") %>%
  left_join(otg_raw_all$discharge_measurements) %>%
  select(path_nm:error_message,
         object_id:station_velocity) %>%
  as.data.frame() #%>%
#View()

# END SCRIPT
