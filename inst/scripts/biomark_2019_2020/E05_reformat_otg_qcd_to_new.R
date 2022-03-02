# Authors: Mike Ackerman
#
# Purpose: Reformat the 2019 & 2020 OTG data to a format
#          that matches the 2018 & 2021 data i.e., to match
#          the newest DASH data collection forms
#
# Created: March 2, 2022
# Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load QC'd OTG data from 2019 & 2020
#-------------------------
path_2019 = paste0(nas_prefix,
                   "Public Data/data/habitat/DASH/OTG/2019")
path_2020 = paste0(nas_prefix,
                   "Public Data/data/habitat/DASH/OTG/2020")

# list of otg_qcd.rda files in path_2019 and path_2020
otg_19_paths = list.files(path = path_2019,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2019, ., sep = "/")

otg_20_paths = list.files(path = path_2020,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2020, ., sep = "/")

otg_1920_paths = c(otg_19_paths, otg_20_paths)

# load each of otg_qcd.rda objects
otg_list = otg_1920_paths %>%
  map(.f = function(x) {
    load(x) %>%
      get()
  })

# combine elements of otg_list into otg
for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg = otg_list[[1]]
  } else {
    otg = suppressMessages(purrr::map2(otg,
                                       otg_list[[i]],
                                       dplyr::full_join))
  }
}

#-------------------------
# reformat to match newest data collection form
#-------------------------
# survey
otg$survey = otg$survey %>%
  mutate(`Stream Name` = c("Big Springs Creek",
                           "Big Timber Creek",
                           "Big Timber Creek",
                           "Big Timber Creek",
                           "Bohannon Creek",
                           "Bohannon Creek",
                           "Canyon Creek",
                           "EF Bohannon Creek",
                           "Hawley Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Kenney Creek",
                           "Kenney Creek",
                           "Little Springs Creek",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "Big Springs Creek",
                           "Lemhi River",
                           "Grouse Creek",
                           "Lake Creek",
                           "Summit Creek"),
         HiddenStart = NA,
         HiddenEnd = NA,
         `Water Temp (C)` = NA) %>%
  rename(`Survey Start Date Time` = `Survey Date`) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Stream Name`,
         `Site Name`,
         `Survey Start Date Time`,
         HiddenStart,
         HiddenEnd,
         `Survey Crew`,
         `Water Temp (C)`,
         `Conductivity (ms)`,
         CreationDate,
         Creator,
         EditDate,
         Editor,
         x,
         y)

# cu
otg$cu = otg$cu %>%
  mutate(TOS = FALSE,
         BOS = FALSE,
         `Cover Sum (%)` = rowSums(select(.,
                                          `Overhanging Cover`,
                                          `Aquatic Vegetation`,
                                          `Woody Debris Cover`,
                                          `Artificial Cover`,
                                          `Total No Cover`)),
         `Ocular Sum (%)` = rowSums(select(.,
                                           `Sand/Fines 2mm`,
                                           `Gravel 2-64mm`,
                                           `Cobble 64-256mm`,
                                           `Boulder 256mm`))) %>%
  rename(`Overhanging (%)` = `Overhanging Cover`,
         `Aquatic Vegetation (%)` = `Aquatic Vegetation`,
         `Woody Debris (%)` = `Woody Debris Cover`,
         `Artificial (%)` = `Artificial Cover`,
         `Total No Cover (%)` = `Total No Cover`,
         `Sand Fines <2mm (%)`= `Sand/Fines 2mm`,
         `Gravel 2-64mm (%)` = `Gravel 2-64mm`,
         `Cobble 64-256mm (%)` = `Cobble 64-256mm`,
         `Boulder >256mm (%)` = `Boulder 256mm`,
         `Width 1 (m)` = `Width 1`,
         `Width 2 (m)` = `Width 2`,
         `Width 3 (m)` = `Width 3`,
         `Width 4 (m)` = `Width 4`,
         `Width 5 (m)` = `Width 5`) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Channel Segment Number`,
         `Channel Unit Number`,
         `Channel Unit Type`,
         `Maximum Depth (m)`,
         `Thalweg Exit Depth (m)`,
         `Channel Unit Notes`,
         TOS,
         BOS,
         `Overhanging (%)`,
         `Aquatic Vegetation (%)`,
         `Woody Debris (%)`,
         `Artificial (%)`,
         `Total No Cover (%)`,
         `Cover Sum (%)`,
         `Sand Fines <2mm (%)`,
         `Gravel 2-64mm (%)`,
         `Cobble 64-256mm (%)`,
         `Boulder >256mm (%)`,
         `Ocular Sum (%)`,
         `Pebble 1 (mm)`:`Pebble 11 (mm)`,
         `Width 1 (m)`:`Width 5 (m)`,
         ParentGlobalID,
         CreationDate,
         Creator,
         EditDate,
         Editor)

# wood
otg$wood = otg$wood %>%
  select(-`Large Wood Number`)

# undercut
otg$undercut = otg$undercut %>%
  select(-`Undercut Number`)

# discharge
otg$discharge = otg$discharge_measurements %>%
  group_by(ParentGlobalID) %>%
  mutate(`Tape Distance (m)` = cumsum(`Station Width`)) %>%
  rename(`Station Depth (m)` = `Station Depth`,
         `Station Velocity (m/s)` = `Station Velocity`) %>%
  mutate(path_nm = str_replace(path_nm, "DischargeMeasurements_6", "Discharge_5")) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Tape Distance (m)`,
         `Station Depth (m)`,
         `Station Velocity (m/s)`,
         ParentGlobalID,
         CreationDate,
         Creator,
         EditDate,
         Editor)

# remove discharge_measurements
otg$discharge_measurements = NULL

saveRDS(otg,
        file = paste0(nas_prefix,
                      "Public Data/data/habitat/DASH/OTG/prepped/otg_qcd_1920_new_format.rds"))

