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
hr_sf = cu_sf %>%
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
            cu_freq = round(n_cu / hr_length_m * 100, 2),
            pool_freq = round(n_pool / hr_length_m * 100, 2),
            run_freq = round(n_run / hr_length_m * 100, 2),
            riffle_freq = round(n_riffle / hr_length_m * 100, 2),
            rapid_freq = round(n_rapid / hr_length_m * 100, 2),
            oca_freq = round(n_oca / hr_length_m * 100, 2),
            ssc_freq = round(n_ssc / hr_length_m * 100, 2),
            slow_freq = round(n_slow / hr_length_m * 100, 2),
            fst_turb_freq = round(n_fst_turb / hr_length_m * 100, 2),
            water_temp_c = unique(site_water_temp_c),
            conductivity_ms = unique(site_conductivity_ms),
            hr_max_depth_m = round(max(maximum_depth_m, na.rm = T), 2),
            overhang_cov_perc = round(weighted.mean(overhanging_percent, cu_length_m), 2),
            aquat_veg_cov_perc = round(weighted.mean(aquatic_vegetation_percent, cu_length_m), 2),
            woody_cov_perc = round(weighted.mean(woody_debris_percent, cu_length_m), 2),
            artificial_cov_perc = round(weighted.mean(artificial_percent, cu_length_m), 2),
            total_cover_perc = sum(overhang_cov_perc, aquat_veg_cov_perc, woody_cov_perc, artificial_cov_perc),
            no_cover_perc = round(weighted.mean(total_no_cover_percent, cu_length_m), 2))




