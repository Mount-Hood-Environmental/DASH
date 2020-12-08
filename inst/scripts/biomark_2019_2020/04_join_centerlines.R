# Authors: Kevin See
#
# Purpose: A script to join OTG data to
# spatial centerlines, making the OTG channel
# unit data spatial
#
# Created: December 8, 2020
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
library(janitor)
library(sf)
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
# read in centerlines
#-------------------------
cl_path = paste0(nas_prefix,
                 "/data/habitat/DASH/centerlines/2019")

cl_list = list.files(path = cl_path,
                     pattern = "^cl_join.shp$",
                     recursive = T) %>%
  paste(cl_path, ., sep = "/") %>%
  as.list() %>%
  map(.f = function(x) {
    read_sf(x)
  })

for(i in 1:length(cl_list)) {
  if(i == 1) {
    cl_sf = cl_list[[i]]
  } else {
    cl_sf = rbind(cl_sf,
                  cl_list[[i]])
  }
}

rm(cl_list)

cl_sf %>%
  select(Site_ID) %>%
  plot()

cl_sf %>%
  filter(is.na(Site_ID) |
           CU_Number == 0 |
           is.na(CU_Type))
cl_sf %>%
  filter(CU_Number == 0)

#-------------------------
# load QC'd OTG data
#-------------------------
otg_path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/2019")

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
# join the OTG data
#-------------------------
cl_sf %>%
  left_join(otg_all$survey)

unique(cl_sf$Site_ID)[unique(cl_sf$Site_ID) %in% unique(otg_all$survey$site_name)]
unique(cl_sf$Site_ID)[!unique(cl_sf$Site_ID) %in% unique(otg_all$survey$site_name)]

unique(otg_all$survey$site_name)[!unique(otg_all$survey$site_name) %in% unique(cl_sf$Site_ID)]

otg_all$cu %>%
  left_join(otg_all$survey %>%
              select(parent_global_id = global_id,
                     site_name,
                     survey_date)) %>%
  filter(site_name == "Barton_2019",
         channel_unit_number == 16) %>%
  as.data.frame()

otg_all$wood %>%
  left_join(otg_all$cu %>%
              select(matches("global_id"),
                     channel_unit_type,
                     channel_unit_number,
                     channel_segment_number) %>%
              left_join(otg_all$survey %>%
                          select(global_id,
                                 site_name,
                                 survey_date),
                        by = c("parent_global_id" = "global_id")),
            by = c("parent_global_id" = "global_id")) %>%
  janitor::tabyl(site_name)
