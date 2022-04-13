# Authors: Mike Ackerman
#
# Purpose: Start to explore the complete OTG dataset and begin
# to roll up measurements into habitat reach metrics
#
# Initially created: April 12, 2022
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(sf)
library(dplyr)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# read in spatial otg
#-------------------------
dash_cu_sf = st_read(dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_18to21.gpkg"))

# do channel unit types match btw otg and channel unit points?
cu_match = dash_cu_sf %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(path_nm,
         site_name,
         year,
         cu_id,
         channel_segment_number,
         channel_unit_number,
         channel_unit_type,
         cu_type) %>%
  mutate(match = if_else(channel_unit_type == cu_type, 1, 0)) %>%
  filter(match == 0)
cu_match # omg, they all match!

# add additional cu metrics
cu_tmp = dash_cu_sf %>%
  # remove cu_type from cu points
  select(-cu_type) %>%
  # calculate residual depth
  mutate(residual_depth_m = maximum_depth_m - thalweg_exit_depth_m)

dash_hr_sf = cu_tmp %>%



