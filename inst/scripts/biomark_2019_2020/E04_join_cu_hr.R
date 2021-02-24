# Authors: Mike Ackerman
#
# Purpose: A script to join CU point data w/
# data defining habitat reaches i.e., which channel
# units should be rolled up together.
#
# Created: February 23, 2021
# Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(sf)
library(janitor)
library(tidyverse)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-------------------------
# read in cu points
#-------------------------
cu_pts = read_sf(paste0(nas_prefix,
                        "/data/habitat/DASH/channel_units/compiled/cu_points_2019_2020.shp"),
                 crs = 4326) %>%
  clean_names()

#-------------------------
# read in habitat reach info
#-------------------------
hr_path = paste0(nas_prefix,
                 "/data/habitat/DASH/habitat_reaches")

# list .csv files
hr_files = list.files(path = hr_path,
                      pattern = "*.csv",
                      recursive = T) %>%
  paste(hr_path, ., sep = "/") %>%
  as.list()

#  read in csv files, remove records with missing x/y
hr_df = hr_files %>%
  map_dfr(read_csv,
          col_types = "dcccccdcccdd") %>%
  clean_names()

# create simple feature
# hr_sf = hr_df %>%
#   # nine records from Little Springs missing lat/lon values
#   filter(!is.na(x) & !is.na(y)) %>%
#   st_as_sf(coords = c('x', 'y'),
#            crs = 4326)

rm(hr_path, hr_files)

#-------------------------
# join above data frames
#-------------------------
cu_hr_sf = cu_pts %>%
  full_join(hr_df,
            by = "global_id",
            suffix = c(".cu", ".hr"))

#-------------------------
# write for review
#-------------------------
write_csv(cu_hr_sf,
          paste0(nas_prefix,
                 "/data/habitat/DASH/channel_units/compiled/cu_hr_sf_raw.csv"))

#-------------------------
# grab most recent OTG data to assist w review
#-------------------------
otg = read_rds(paste0(nas_prefix,
                      "/data/habitat/DASH/OTG/prepped/dash_1920_cu_no_impute.rds"))

#-------------------------
# begin data cleaning
#-------------------------
tabyl(otg$site_name)



