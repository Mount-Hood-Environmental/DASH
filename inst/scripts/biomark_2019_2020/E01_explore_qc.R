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
                    pattern = "\\qc_results.rds$",
                    # pattern = "^qc_results.*\\.csv$",
                    recursive = T) %>%
  paste0(otg_path, .) %>%
  as.list() %>%
  map_df(read_csv) %>%
  clean_names()


qc_all = list.files(path = otg_path,
           pattern = "^qc_final.rds",
           recursive = T) %>%
  paste0(otg_path, .) %>%
  as.list() %>%
  map(.f = function(x) {
    load(x) %>%
      get() #%>%
      # map(clean_names)
  }) %>%
  map_df(.f = identity) %>%
  clean_names()

#-------------------------
# load all OTG data
#-------------------------

# list of otg files in otg_path (either raw or QC'd)
otg_files = list.files(path = otg_path,
                       # pattern = "^otg_raw\\.rda$",
                       pattern = "^otg_qcd\\.rda$",
                       recursive = T) %>%
  paste0(otg_path, .)

# load the otg_raw files, and join them together
otg_list = as.list(otg_files) %>%
  map(.f = function(x) {
    load(x) %>%
      get() %>%
      map(clean_names)
  })

for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg_all = otg_list[[1]]
  } else {
    otg_all = suppressMessages(purrr::map2(otg_all,
                                           otg_list[[i]],
                                           dplyr::full_join))
  }
}

# clean up
rm(otg_files,
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
  arrange(desc(n))

### Survey ###
# latitude, longitude flags
qc_all %>%
  filter(source == "Survey") %>%
  left_join(otg_all$survey) %>%
  select(source:error_message, x:y)

### CU ###
# `Maximum Depth (m)` errors
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  select(source:error_message) %>%
  left_join(otg_all$cu) %>%
  select(source:error_message, channel_unit_type, maximum_depth_m) %>%
  # View()
  janitor::tabyl(channel_unit_type)

# Duplicate channel units
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("appears more than once", error_message)) %>%
  mutate(channel_unit_number = str_extract(error_message, "[:digit:]+")) %>%
  mutate(across(channel_unit_number,
                as.numeric)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$cu) %>%
  arrange(path_nm, channel_unit_number) #%>%
#View()

# Thalweg exit depth
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Thalweg exit depth", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$cu) %>%
  select(source:error_message,
         channel_unit_number,
         channel_segment_number,
         thalweg_exit_depth_m)

# Ocular substrate estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$cu) %>%
  select(source:global_id,
         channel_unit_type,
         sand_fines_2mm:boulder_256mm) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_all %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$cu) %>%
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
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$cu) %>%
  select(path_nm:global_id,
         contains("pebble")) #%>%
#View()

### Wood ###
qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Large Wood Number is <blank> or NA.", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$wood)

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Diameter", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$wood) %>%
  select(path_nm:global_id,
         object_id:ballasted) #%>%
#View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$wood) %>%
  select(path_nm:global_id,
         object_id:ballasted) %>%
  pivot_longer(wet:ballasted) %>%
  filter(is.na(value)) #%>%
#View()

qc_all %>%
  filter(source == "Wood") %>%
  filter(grepl("Length is less than or equal to the diameter|falls outside of the expected", error_message)) %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$wood) %>%
  select(path_nm:global_id,
         length_m,
         diameter_m)

### Jam ###
qc_all %>%
  filter(source == "Jam") %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$jam) %>%
  select(-(parent_global_id:editor)) %>%
  as.data.frame() #%>%
#View()

### Undercut ###
qc_all %>%
  filter(source == "Undercut") %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$undercut) %>%
  select(path_nm:error_message,
         undercut_number:width_75_percent_m) %>%
  as.data.frame() #%>%
# View()

# Discharge measurements
qc_all %>%
  filter(source == "DischargeMeasurements") %>%
  select(-location_id,
         -parent_global_id) %>%
  left_join(otg_all$discharge_measurements) %>%
  select(path_nm:error_message,
         object_id:station_velocity) %>%
  as.data.frame() #%>%
#View()

# END SCRIPT
