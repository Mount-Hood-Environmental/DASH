# Authors: Mike Ackerman
#
# Purpose: Reformat the channel unit point data from
# 2018 - 2020 to match the new data collection form
# in Field Maps
#
# Initially created: March 30, 2022
#   Modified: April 8, 2021
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(sf)
library(janitor)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------------------
# read in DASH Field Maps channel unit points data
#-------------------------------------
cu_points_path = paste0(nas_prefix,
                        "main/data/habitat/DASH/channel_units")

# read in and slightly reformat 2018 - 2020 channel unit points
cu_pts_1820 = st_read(paste0(cu_points_path, "/dash_cu_points_18.shp")) %>%
  as_tibble() %>%
  rbind(st_read(paste0(cu_points_path, "/dash_cu_points_19.shp"))) %>%
  rbind(st_read(paste0(cu_points_path, "/dash_cu_points_20.shp"))) %>%
  rename(global_id = globl_d,
         stream_name = strm_nm,
         site_name = site_nm,
         fish_site = fsh_st_) %>%
  select(-c(sort_id, objectd),
         -c(tos, bos)) %>%
  select(global_id,
         year,
         stream_name,
         site_name,
         fish_site,
         seg_num,
         cu_num,
         cu_type,
         hab_rch,
         grts_id,
         geometry,
         notes)

# read in and slightly reformat 2021 channel units to match 2018 - 2020
cu_pts_21 = st_read(paste0(cu_points_path, "/dash_cu_points_21.shp")) %>%
  as_tibble() %>%
  clean_names() %>%
  rename(stream_name = strm_nm,
         site_name = site_nm,
         year = srvy_year) %>%
  select(-(creation_da:editor)) %>%
  mutate(fish_site = NA,
         grts_id = NA) %>%
  select(global_id,
         year,
         stream_name,
         site_name,
         fish_site,
         seg_num,
         cu_num,
         cu_type,
         hab_rch,
         grts_id,
         geometry,
         notes)

# bind them together and change back to sf object
cu_pts = rbind(cu_pts_1820, cu_pts_21) %>%
  st_as_sf()

# and write back out to NAS, but as geopackage
st_write(cu_pts,
         dsn = paste0(cu_points_path, "/compiled/dash_cu_points.gpkg"),
         delete_dsn = T)

# END SCRIPT
