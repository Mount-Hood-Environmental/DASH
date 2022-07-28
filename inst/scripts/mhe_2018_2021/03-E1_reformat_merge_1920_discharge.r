# Authors: Mark Roes
#
# Purpose: Reformat and merge 2019 and 2020 discharge data for 04_explore_otg
#
# Created: July 27, 2022
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(plyr)
library(sf)
library(magrittr)
library(DASH)

#-----------------------------
# set some arguments/parameters
#-----------------------------
#set nas prefix
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

# path to OTG data on the NAS
otg_path = paste0(nas_prefix, "main/data/habitat/DASH/OTG/")

#-------------------------
# load all OTG data
#-------------------------
otg = readRDS(file = paste0(otg_path,
                            "prepped/otg_all_18to21.rds"))
# remove the qc_results data frame from otg
otg$qc_results = NULL

#Load discharge header info
yr_wtsd = c("2019/lemhi/",
            "2019/nf_salmon/",
            "2020/lemhi/",
            "2020/secesh/")
path = c()

for (yw in yr_wtsd) {
  path = c(path, list.dirs(paste0(otg_path, yw, "1_formatted_csvs"))[-1])
}

id = ldply(paste0(path, "/Discharge_5.csv"), read_csv)

dis_1920 = id %>%
  select(GlobalID, ParentGlobalID) %>%
  left_join(otg$discharge, by = c("GlobalID" = "ParentGlobalID")) %>%
  select(path_nm, ObjectID, GlobalID, `Tape Distance (m)`, `Station Depth (m)`, `Station Velocity (m/s)`, ParentGlobalID, CreationDate, Creator, EditDate, Editor) %>%
  mutate(GlobalID = "ParentID_is_Site") %>%
  janitor::clean_names() %>%
  #Issues with data (TapeDist vs. StatWidth)leading to weird discharge values.
  rollup_cu_discharge()

  View(filter(dis_1920, ParentGlobalID == "a8ff9117-d94c-4b95-a488-66e809b8fae5") %>%
         mutate(vel =`Tape Distance (m)` * `Station Depth (m)` * `Station Velocity (m/s)` ))

save(dis_1920,
     file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_discharge_1920.rda"))


#MERGE TESTING: Append the discharge data onto the CU data at the site level
otg_sf = st_read(dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_18to21.gpkg"))

setdiff( dis_1920$parent_global_id, otg_sf$parent_global_id)

load(paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_discharge_1920.rda"))

otg_sf %<>%
  left_join(dis_1920, by = "parent_global_id", suffix = c("","_new")) %>%
  mutate(discharge_cms = ifelse(is.na(discharge_cms),discharge_cms_new,discharge_cms),
         discharge_cfs = ifelse(is.na(discharge_cfs),discharge_cfs_new,discharge_cfs)) %>%
  select(-discharge_cms_new, -discharge_cfs_new)

