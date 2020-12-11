# Authors: Kevin See
#
# Purpose: rollup all OTG data to channel unit scale
#
# Created: December 9, 2020
# Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(sf)
#library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-------------------------
# load QC'd OTG data
#-------------------------
otg_path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/2019")
                  # "/data/habitat/DASH/OTG/2020")

# list of otg_raw files in otg_path
otg_list = list.files(path = otg_path,
                      pattern = "^otg_qcd.rda$",
                      recursive = T) %>%
  paste(otg_path, ., sep = "/") %>%
  as.list() %>%
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
rm(otg_list)

#-------------------------
# roll up all the OTG data to CU scale
#-------------------------
cu_df = rollup_channel_unit(otg_all$cu,
                            otg_all$survey,
                            otg_all$jam,
                            otg_all$undercut,
                            otg_all$wood,
                            otg_all$discharge,
                            otg_all$discharge_measurements)

#-------------------------
# QC the rollup
#-------------------------
qc_roll = qc_rollup(otg_all$cu,
                    otg_all$survey,
                    otg_all$jam,
                    otg_all$undercut,
                    otg_all$wood,
                    otg_all$discharge,
                    otg_all$discharge_measurements)

qc_roll$error_df

# what do the individual data look like for these dangling channel units?
qc_roll$miss_rollup %>%
  filter(source == "Wood") %>%
  left_join(otg_all$wood)

qc_roll$miss_rollup %>%
  filter(source == "Jam") %>%
  left_join(otg_all$jam)

qc_roll$miss_rollup %>%
  filter(source == "Undercut") %>%
  left_join(otg_all$undercut)

# where are the dangling discharge measurements?
qc_roll$miss_discharge


#-------------------------
# extra junk, to be deleted later
#-------------------------

cu_df %>%
  filter(parent_global_id == "c03aac81-2be9-4365-a5c4-94ce7abd5b3b") %>%
  mutate(across(channel_unit_number,
                as.numeric)) %>%
  pull(channel_unit_number) %>%
  range()

cu_df %>%
  filter(parent_global_id == "c03aac81-2be9-4365-a5c4-94ce7abd5b3b") %>%
  filter(channel_unit_number %in% 215:220)

cl_sf %>%
  filter(grepl('BigTimber', Site_ID),
         CU_Number %in% 115:120)

cu_main %>%
  filter(channel_unit_number == 317) %>%
  select(site_name:channel_unit_notes)
