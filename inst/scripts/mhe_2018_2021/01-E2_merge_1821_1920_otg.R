# Authors: Mike Ackerman
#
# Purpose: Join the 2019/2020 data (old format) to 2018/2021 data (new format)
# and then rollup OTG data to channel-unit scale
#
# Created: March 2, 2022
# Last Modified: March 23, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(janitor)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load QC'd OTG data from 2018 & 2021
#-------------------------
path_2018 = paste0(nas_prefix,
                   "main/data/habitat/DASH/OTG/2018")
path_2021 = paste0(nas_prefix,
                   "main/data/habitat/DASH/OTG/2021")

# list of otg_qcd.rda files in path_2019 and path_2020
otg_18_paths = list.files(path = path_2018,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2018, ., sep = "/")

otg_21_paths = list.files(path = path_2021,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2021, ., sep = "/")

otg_1821_paths = c(otg_18_paths, otg_21_paths)

# load each of otg_qcd.rda objects
otg_list = otg_1821_paths %>%
  map(.f = function(x) {
    load(x) %>%
      get()
  })

# clean some stuff
rm(otg_18_paths, otg_21_paths,
   path_2018, path_2021,
   otg_1821_paths)

# add some blank jam tibbles for a couple cases where there are no data
otg_list[[2]]$jam = tibble("path_nm" = as.character(),
                           "ObjectID" = as.integer(),
                           "GlobalID" = as.character(),
                           "Length (m)" = as.double(),
                           "Width (m)" = as.double(),
                           "Height (m)" = as.double(),
                           "Estimated Number of Pieces" = as.integer(),
                           "ParentGlobalID" = as.character(),
                           "CreationDate" = as.character(),
                           "Creator" = as.character(),
                           "Edit Date" = as.character(),
                           "Editor" = as.character())
otg_list[[3]]$jam = tibble("path_nm" = as.character(),
                           "ObjectID" = as.integer(),
                           "GlobalID" = as.character(),
                           "Length (m)" = as.double(),
                           "Width (m)" = as.double(),
                           "Height (m)" = as.double(),
                           "Estimated Number of Pieces" = as.integer(),
                           "ParentGlobalID" = as.character(),
                           "CreationDate" = as.character(),
                           "Creator" = as.character(),
                           "EditDate" = as.character(),
                           "Editor" = as.character())
otg_list[[2]] = otg_list[[2]][c("survey", "cu", "wood", "jam", "undercut", 'discharge', "qc_results")]
otg_list[[3]] = otg_list[[3]][c("survey", "cu", "wood", "jam", "undercut", 'discharge', "qc_results")]

# combine elements of otg_list into otg
for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg_1821 = otg_list[[1]]
  } else {
    otg_1821 = suppressMessages(purrr::map2(otg_1821,
                                       otg_list[[i]],
                                       dplyr::full_join))
  }
}

#-------------------------
# load QC'd OTG data from 2019 & 2020
#-------------------------
otg_1920 = readRDS(file = paste0(nas_prefix,
                                 "main/data/habitat/DASH/OTG/prepped/otg_qcd_1920_new_format.rds"))

#-------------------------
# bind 18/21 & 19/20 OTG datasets
#-------------------------
otg_all_list = list(otg_1821, otg_1920)
for(i in 1:length(otg_all_list)) {
  if(i == 1) {
    otg_all = otg_all_list[[1]]
  } else {
    otg_all = suppressMessages(purrr::map2(otg_all,
                                           otg_all_list[[i]],
                                           dplyr::full_join))
  }
}

# save results
saveRDS(otg_all,
        file = paste0(nas_prefix,
                      "main/data/habitat/DASH/OTG/prepped/otg_all_18to21.rds"))

# END SCRIPT
