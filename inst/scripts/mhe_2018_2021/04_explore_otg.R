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
  mutate(resid_depth_m = maximum_depth_m - thalweg_exit_depth_m) %>%
  # diameter metrics
  rowwise() %>%
  mutate(
    cu_d16 = quantile(c_across(starts_with("pebble")), 0.16, na.rm = T),
    cu_d50 = quantile(c_across(starts_with("pebble")), 0.50, na.rm = T),
    cu_d84 = quantile(c_across(starts_with("pebble")), 0.84, na.rm = T),
  )

# write channel unit sf object to geodatabase
st_write(cu_sf,
         dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_cu_18-21.gpkg"),
         delete_dsn = T)

#-------------------------
# initiate habitat reach sf
#-------------------------
hr_sf = cu_sf %>%
  group_by(site_name, year, hab_rch) %>%
  summarise(
    hr_length_m = sum(cu_length_m),
    # channel unit counts
    cu_ids = list(unique(cu_id)),
    n_cu = n_distinct(cu_id),
    n_seg = n_distinct(channel_segment_number),
    n_pool = sum(channel_unit_type == "Pool"),
    n_run = sum(channel_unit_type == "Run"),
    n_riffle = sum(channel_unit_type == "Riffle"),
    n_rapid = sum(channel_unit_type == "Rapid+"),
    n_oca = sum(channel_unit_type == "OCA"),
    n_ssc = sum(channel_unit_type == "SSC"),
    n_slow = sum(n_pool, n_oca),
    n_fst_turb = sum(n_riffle, n_rapid),
    # channel unit frequencies
    cu_freq = round(n_cu / hr_length_m * 100, 2),
    pool_freq = round(n_pool / hr_length_m * 100, 2),
    run_freq = round(n_run / hr_length_m * 100, 2),
    riffle_freq = round(n_riffle / hr_length_m * 100, 2),
    rapid_freq = round(n_rapid / hr_length_m * 100, 2),
    oca_freq = round(n_oca / hr_length_m * 100, 2),
    ssc_freq = round(n_ssc / hr_length_m * 100, 2),
    slow_freq = round(n_slow / hr_length_m * 100, 2),
    fst_turb_freq = round(n_fst_turb / hr_length_m * 100, 2),
    # channel unit percents, by length
    pool_perc = round((sum(cu_length_m[channel_unit_type == "Pool"]) / hr_length_m) * 100, 1),
    run_perc = round((sum(cu_length_m[channel_unit_type == "Run"]) / hr_length_m) * 100, 1),
    riffle_perc = round((sum(cu_length_m[channel_unit_type == "Riffle"]) / hr_length_m) * 100, 1),
    rapid_perc = round((sum(cu_length_m[channel_unit_type == "Rapid+"]) / hr_length_m) * 100, 1),
    oca_perc = round((sum(cu_length_m[channel_unit_type == "OCA"]) / hr_length_m) * 100, 1),
    ssc_perc = round((sum(cu_length_m[channel_unit_type == "SSC"]) / hr_length_m) * 100, 1),
    slow_perc = sum(pool_perc, oca_perc),
    fst_turb_perc = sum(riffle_perc, rapid_perc),
    # fish cover
    fish_cov_tr_veg = round(weighted.mean(overhanging_percent, cu_length_m), 1),
    fish_cov_aq_veg = round(weighted.mean(aquatic_vegetation_percent, cu_length_m), 1),
    fish_cov_lwd = round(weighted.mean(woody_debris_percent, cu_length_m), 1),
    fish_cov_art = round(weighted.mean(artificial_percent, cu_length_m), 1),
    fish_cov_total = sum(fish_cov_tr_veg, fish_cov_aq_veg, fish_cov_lwd, fish_cov_art),
    fish_cov_none = round(weighted.mean(total_no_cover_percent, cu_length_m), 1),
    # size
    hr_max_depth_m = round(max(maximum_depth_m, na.rm = T), 1),
    # substrate ocular estimates
    sub_est_sand_fines = round(weighted.mean(sand_fines_2mm_percent, cu_length_m), 1),
    sub_est_gravl = round(weighted.mean(gravel_2_64mm_percent, cu_length_m), 1),
    sub_est_cbl = round(weighted.mean(cobble_64_256mm_percent, cu_length_m), 1),
    sub_est_bldr = round(weighted.mean(boulder_256mm_percent, cu_length_m), 1),
    # diameter estimates from pebble counts
    hr_d16_mm = mean(cu_d16, na.rm = T),
    hr_d50_mm = mean(cu_d50, na.rm = T),
    hr_d84_mm = mean(cu_d84, na.rm = T),
    # wood

    # undercuts

    # temperature
    obs_water_temp_c = unique(site_water_temp_c),
    # water quality
    obs_conductivity_ms = unique(site_conductivity_ms),
    .groups = "drop"
  )

# write habitat reach sf object to file
saveRDS(hr_sf,
        file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_hr_18-21.rds"))

