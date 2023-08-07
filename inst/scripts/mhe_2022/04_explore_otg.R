# Authors: Mike Ackerman
#
# Purpose: Start to explore the complete OTG dataset and begin
# to roll up measurements into habitat reach metrics
#
# Initially created: April 12, 2022
#   Last Modified: August 2, 2023 by Tulley Mackey for DASH data collected near
#                  the Lemhi-Hayden confluence in July 2022
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
library(elevatr)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# read in spatial otg
#-------------------------
otg_sf = st_read(dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_2022.shp")) %>%
  mutate(site_ln = ifelse(site_ln == -113.820058, -113.62607, site_ln),
         site_lt = ifelse(site_lt == 45.13898738, 44.869823, site_lt)) %>%
  rename(maximum_depth_m = mxmm_d_,
         thalweg_exit_depth_m = thlw___)



#-----------------------
# pull in elevation data from UGSG - 3DEP - CURRENTLY NOT IN USE
#-----------------------
# elev_DEM = otg_sf %>%
#  select(x = site_ln,
#         y = site_lt)

# elevtr() needs a data frame with x & y coordinates
# out <- get_elev_point(locations = as.data.frame(elev_DEM), units ="meters", src = "epqs", prj = 4326) #WGS84

# otg_sf = otg_sf %>%
#  mutate(elev_m_dem = out@data[["elevation"]])

#-------------------------
# additional metrics for channel units
#-------------------------
cu_sf = otg_sf %>%
  # residual depth
  mutate(resid_depth_m = maximum_depth_m - thalweg_exit_depth_m) %>%
  # diameter metrics
  rowwise() %>%
  mutate(
    cu_d16 = quantile(c_across(starts_with("pbbl")), 0.16, na.rm = T),
    cu_d50 = quantile(c_across(starts_with("pbbl")), 0.50, na.rm = T),
    cu_d84 = quantile(c_across(starts_with("pbbl")), 0.84, na.rm = T),  )

# write channel unit sf object to geodatabase
st_write(cu_sf,
         dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_cu_22.shp"),
         delete_dsn = T)

#-------------------------
# initiate habitat reach sf
#-------------------------
hr_sf = cu_sf %>%
  group_by(site_nm, hab_rch) %>%
  reframe(
    hr_length_m = round(sum(c_lngt_), 1),
    # channel unit counts
    #cu_ids = list(unique(cu_id)),
    n_cu = n_distinct(cu_id),
    n_seg = n_distinct(chnnl_s_),
    n_pool = sum(chnnl_nt_t == "Pool"),
    n_run = sum(chnnl_nt_t == "Run"),
    n_riffle = sum(chnnl_nt_t == "Riffle"),
    n_rapid = sum(chnnl_nt_t == "Rapid+"),
    n_oca = sum(chnnl_nt_t == "OCA"),
    n_ssc = sum(chnnl_nt_t == "SSC"),
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
    pool_perc = round((sum(c_lngt_[chnnl_nt_t == "Pool"]) / hr_length_m) * 100, 1),
    run_perc = round((sum(c_lngt_[chnnl_nt_t == "Run"]) / hr_length_m) * 100, 1),
    riffle_perc = round((sum(c_lngt_[chnnl_nt_t == "Riffle"]) / hr_length_m) * 100, 1),
    rapid_perc = round((sum(c_lngt_[chnnl_nt_t == "Rapid+"]) / hr_length_m) * 100, 1),
    oca_perc = round((sum(c_lngt_[chnnl_nt_t == "OCA"]) / hr_length_m) * 100, 1),
    ssc_perc = round((sum(c_lngt_[chnnl_nt_t == "SSC"]) / hr_length_m) * 100, 1),
    slow_perc = sum(pool_perc, oca_perc),
    fst_turb_perc = sum(riffle_perc, rapid_perc),
    pool_turb_ratio = round(sum(c_lngt_[chnnl_nt_t == "Pool"]) / sum(c_lngt_[chnnl_nt_t == "Riffle"], c_lngt_[chnnl_nt_t == "Rapid+"]), 2),
    # fish cover
    fish_cov_tr_veg = round(weighted.mean(ovrhng_, c_lngt_), 1),
    fish_cov_aq_veg = round(weighted.mean(aqtc_v_,c_lngt_), 1),
    fish_cov_lwd = round(weighted.mean(wdy_db_, c_lngt_), 1),
    fish_cov_art = round(weighted.mean(artfcl_, c_lngt_), 1),
    fish_cov_total = sum(fish_cov_tr_veg, fish_cov_aq_veg, fish_cov_lwd, fish_cov_art),
    fish_cov_none = round(weighted.mean(ttl_n__, c_lngt_), 1),
    # substrate ocular estimates
    sub_est_sand_fines = round(weighted.mean(snd__2_, c_lngt_), 1),
    sub_est_gravl = round(weighted.mean(g_2_64_, c_lngt_), 1),
    sub_est_cbl = round(weighted.mean(c_64_25, c_lngt_), 1),
    sub_est_bldr = round(weighted.mean(bl_256_, c_lngt_), 1),
    sub_est_cbl_bldr = sum(sub_est_cbl, sub_est_bldr),
    # diameter estimates from pebble counts
    hr_d16_mm = round(mean(cu_d16, na.rm = T), 1),
    hr_d50_mm = round(mean(cu_d50, na.rm = T), 1),
    hr_d84_mm = round(mean(cu_d84, na.rm = T), 1),
    # wood
    n_lwd_bankfull = sum(lwd_n),
    n_lwd_wetted = sum(lwd_n_w),
    lwd_wetted_freq = round(n_lwd_wetted / hr_length_m * 100,2),
    lwd_bankfull_area_m2 = sum(lwd_r_2),
    lwd_wetted_area_m2 = sum(lwd_r_w_2),
    lwd_bankfull_vol_m3 = sum(lwd_v_3),
    lwd_wetted_vol_m3 = sum(lwd_vl_w_3),
    lwd_bankfull_slow_vol_m3 = sum(lwd_v_3[chnnl_nt_t == "Pool"], lwd_v_3[chnnl_nt_t == "OCA"]),
    lwd_wetted_slow_vol_m3 = sum(lwd_vl_w_3[chnnl_nt_t == "Pool"], lwd_vl_w_3[chnnl_nt_t == "OCA"]),
    # jams
    n_jam = sum(jam_n),
    n_jam_pieces = sum(jm_st__),
    jam_area_m2 = sum(jm_r_m2),
    jam_vol_m3 = sum(jm_vl_3),
    # all wood
    n_lwd_all = sum(n_lwd_bankfull, jm_st__),
    wood_all_area_m2 = sum(lwd_bankfull_area_m2, jm_r_m2),
    wood_all_vol_m3 = sum(lwd_bankfull_vol_m3, jm_vl_3),
    # undercuts
    undct_length_m = sum(undct_l_),
    undct_area_m2 = sum(undc__2),
    undct_perc = round(undct_l_ / hr_length_m * 100, 1),
    # complexity
    hr_ms_sin_cl = round(weighted.mean(c_sn_cl[chnnl_s_ == "01"], c_lngt_[chnnl_s_ == "01"], na.rm = T), 2),
    hr_sin_cl = round(weighted.mean(c_sn_cl, c_lngt_, na.rm = T), 2),
    hr_thlwg_dpth_cv = round(raster::cv(thalweg_exit_depth_m, na.rm = T), 2),
    hr_braidedness = round(hr_length_m / sum(c_strg_[chnnl_s_ == "01"]), 2),
    # size
    hr_discharge_cfs = round(mean(dschrg_cf, na.rm = T), 2),
    hr_thlwg_dpth_avg_m = round(mean(thalweg_exit_depth_m, na.rm = T), 2),
    hr_max_depth_m = round(max(maximum_depth_m, na.rm = T), 2),
    hr_avg_pool_dpth_m = mean(maximum_depth_m[chnnl_nt_t == "Pool"]),
    hr_avg_resid_pool_dpth_m = mean(resid_depth_m[chnnl_nt_t == "Pool"]),
    geometry) %>%
    # temperature
    # obs_water_temp_c = unique(site_water_temp_c),
    # water quality
    # obs_conductivity_ms = unique(site_conductivity_ms),
    # elevation
    # elev_m_dem = unique(elev_m_dem),
    # .groups = "drop") %>%
  #st_cast("MULTILINESTRING")
  distinct(hr_length_m, .keep_all = TRUE)


# write habitat reach sf object to file
saveRDS(hr_sf,
        file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_hr_22.rds"))

write_csv(hr_sf,
          file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_hr_22.csv"))

 # write habitat reach sf as shapefile
 st_write(hr_sf,
          dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_hr_22.shp"),
          delete_dsn = T)

#-------------------------
# Append habitat reach data to channel unit object for winter QRF model
#-------------------------

cu_sf %<>%
  left_join(hr_sf %>%
              st_drop_geometry() %>%
              select(site_nm, hab_rch, cu_freq, hr_sin_cl, hr_braidedness)
            ,by = c("site_nm", "hab_rch")
  ) %>%
  mutate(fish_cov_lwd = lwd_r_w_2/(mean(wdth_1_, wdth_2_, wdth_3_, wdth_4_, wdth_5_, na.rm = T)*c_lngt_),
         SubEstCandBldr = c_64_25 + bl_256_
  )

#Write out channel unit data to .csv, .rds, .gpkg
saveRDS(cu_sf,
        file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_cu_22.rds"))

write_csv(cu_sf,
          file = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_cu_22.csv"))

# write channel unit sf object to geodatabase
st_write(cu_sf,
         dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/dash_cu_22.shp"),
         delete_dsn = T)
