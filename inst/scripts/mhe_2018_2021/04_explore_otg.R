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
otg_sf = st_read(dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_18to21.gpkg"))

#-------------------------
# additional metrics for channel units
#-------------------------
cu_sf = otg_sf %>%
  # residual depth
  mutate(resid_depth_m = maximum_depth_m - thalweg_exit_depth_m)

#-------------------------
# initiate habitat reach sf
#-------------------------
hr_sf = otg_sf %>%
  group_by(site_name, year, hab_rch) %>%
  summarise(cu_ids = list(unique(cu_id)),
            n_cus = n_distinct(cu_id),
            n_pools = sum(channel_unit_type == "Pool"),
            n_runs = sum(channel_unit_type == "Run"),
            n_riffles = sum(channel_unit_type == "Riffle"),
            n_rapids = sum(channel_unit_type == "Rapid+"),
            n_ocas = sum(channel_unit_type == "OCA"),
            n_sscs = sum(channel_unit_type == "SSC"),
            n_slow = sum(n_pools, n_ocas),
            n_fst_turb= sum(n_riffles, n_rapids))



