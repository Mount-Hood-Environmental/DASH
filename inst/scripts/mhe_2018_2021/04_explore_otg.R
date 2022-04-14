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
            n_cu = n_distinct(cu_id),
            n_pool = sum(channel_unit_type == "Pool"),
            n_run = sum(channel_unit_type == "Run"),
            n_riffle = sum(channel_unit_type == "Riffle"),
            n_rapid = sum(channel_unit_type == "Rapid+"),
            n_oca = sum(channel_unit_type == "OCA"),
            n_ssc = sum(channel_unit_type == "SSC"),
            n_slow = sum(n_pool, n_oca),
            n_fst_turb = sum(n_riffle, n_rapid),
            hr_length_m = sum(cu_length_m),
            cu_freq = n_cu / hr_length_m * 100,
            pool_freq = n_pool / hr_length_m * 100,
            run_freq = n_run / hr_length_m * 100,
            riffle_freq = n_riffle / hr_length_m * 100,
            rapid_freq = n_rapid / hr_length_m * 100,
            oca_freq = n_oca / hr_length_m * 100,
            ssc_freq = n_ssc / hr_length_m * 100,
            slow_freq = n_slow / hr_length_m * 100,
            fst_turb_freq = n_fst_turb / hr_length_m * 100)



