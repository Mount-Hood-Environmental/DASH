# Authors: Kevin See & Mike Ackerman
#
# Purpose: rollup all OTG data to channel unit scale
#
# Created: December 9, 2020
# Last Modified: January 20, 2021
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
library(DASH)

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
                  "/data/habitat/DASH/OTG/")

# list of otg_qcd files in otg_path
# take advantage of fact that 2018 data doesn't contain a "^otg_qcd.rda$"
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

# combine elements of otg_list into otg_all
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


# clean site names
otg_all$survey = otg_all$survey %>%
  mutate(across(site_name,
                ~ str_replace(., "_.*", "")),
         across(site_name,
                ~str_remove(., " ")))
tabyl(otg_all$survey,
      site_name)

#-------------------------
# roll up all the OTG data to CU scale
#-------------------------
cu_df = otg_to_cu(otg_all$survey,
                  otg_all$cu,
                  otg_all$jam,
                  otg_all$undercut,
                  otg_all$wood,
                  otg_all$discharge,
                  otg_all$discharge_measurements)

# save as prepped data
write_csv(cu_df,
          paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/prepped/dash_2019_2020_otg_cu.csv"))

write_rds(cu_df,
          paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/prepped/dash_2019_2020_otg_cu.rds"))

#-------------------------
# QC the rollup
#-------------------------
qc_roll = qc_rollup(otg_all$survey,
                    otg_all$cu,
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
