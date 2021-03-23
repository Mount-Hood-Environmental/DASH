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

rm(hr_df)
# At this point, I copy/pasted the above file, did a thorough review/QC on the new file, and
# made several changes

#------------------------
# read in the new cu points, overwriting cu_pts
#-------------------------
cu_pts = read_csv(paste0(nas_prefix,
                         "/data/habitat/DASH/channel_units/compiled/dash_cu_points_1920.csv"))

cu_sf = cu_pts %>%
  mutate(geometry = gsub('[c()]', '', geometry)) %>%
  separate(col = geometry,
           into = c('lon', 'lat'),
           sep = '\\,') %>%
  mutate_at('lon', as.double) %>%
  mutate_at('lat', as.double) %>%
  st_as_sf(coords = c('lon', 'lat'),
           crs = 4326)

st_write(cu_sf,
         dsn = paste0(nas_prefix, "/data/habitat/DASH/channel_units/compiled/dash_cu_points_1920.shp"),
         append = T)

#------------------------
# grab most recent OTG data to assist w review
#------------------------
otg = read_rds(paste0(nas_prefix,
                      "/data/habitat/DASH/OTG/prepped/dash_1920_cu_no_impute.rds"))

#------------------------
# join some of cu_pts and otg for further QC
#------------------------
cu_join = cu_pts %>%
  select(site_name,
         year,
         cu_type,
         seg_num,
         cu_num,
         hab_reach) %>%
  mutate(site_name = str_remove_all(site_name, " ")) %>%
  mutate(cu_id = paste0(site_name,
                        "_",
                        str_pad(seg_num, 2, pad = "0"),
                        "_",
                        str_pad(cu_num, 3, pad = "0"))) %>%
  distinct()

otg_join = otg %>%
  select(site_name,
         cu_id,
         channel_unit_type,
         channel_segment_number,
         channel_unit_number)

#-------------------------
# join above data frames
#-------------------------
otg_cu_df = otg_join %>%
  full_join(cu_join,
            by = "cu_id",
            suffix = c(".otg", ".cu"))

#-------------------------
# write for review
#-------------------------
# write_csv(otg_cu_df,
#           paste0(nas_prefix,
#                  "/data/habitat/DASH/channel_units/compiled/otg_cu_df_20210308.csv"))



