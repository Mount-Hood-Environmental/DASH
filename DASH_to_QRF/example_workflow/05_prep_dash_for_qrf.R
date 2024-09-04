# Author: Bryce Oldemeyer, Tulley Mackey
# Purpose: Summarise Dash Data to feed into QRF model
# Created: 1/30/2024
# Modified by: Bryce Oldemeyer, Mark Roes, and Mike Ackerman, Bridger Bertram

# clear environment
rm(list = ls())

# libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)
library(readxl)
library(here)
library(patchwork)
library(ggforce)
library(stringdist)

#-------------------------
# set NAS prefix (depending on operating system), set year and watershed of your site,
# and read in Habitat data.
#-------------------------

data_directory <- here("data/example_data")
#data_directory <- here("data/project_data")


#Read in Channel Unit and Habitat Reach data...
hab_reach = readRDS(paste0(data_directory,"/6_otg_scaled/hab_reach.rds"))
chnl_unit = readRDS(paste0(data_directory,"/6_otg_scaled/chnl_unit.rds"))

##-------------------------
#  Append Temperature Data From reach_200 file
#
#  Note: Project CRS, and watershed boundary are specific to Lemhi-Hayden complex.
#        Will need to modify this step for other sites outside of Lemhi WS Basin.
#        All upper Salmon Watershed boundaries can be found on the NAS
##-------------------------

WS_crs = st_crs(4326) # WGS84

#specify watershed boundary.
ws_bound = st_read(paste0("S:/main/data/habitat/watershed_boundaries/Lemhi_WS.gpkg")) %>%
st_transform(WS_crs)

# Append temperatures from rch_200 file
load("S:/main/data/qrf/gitrepo_data/input/rch_200.rda")

# Grab needed mets from rch_200
temps = rch_200 %>%
  st_transform(crs = WS_crs) %>%
  select(S2_02_11,
         end_elev,
         region,
         p_accum,
         slope) %>%
  rename(avg_aug_temp = S2_02_11)

# Trim down to specified watershed boundary
temps_trim <- temps[ws_bound, ] # This is too easy of a way to clip/subset spatial data.


# add temps_trim to chnl_unit_data
chnl_unit <- chnl_unit %>%
  st_as_sf(coords = c("lat","lon")) %>%
  st_transform(crs = WS_crs) %>%
  st_join(temps_trim,
          left = T,
          join = st_nearest_feature)

# add temps_trim to hab_reach_data
hab_reach <- hab_reach %>%
  mutate(geom_lat = lat,
         geom_lon = lon) %>%
  st_as_sf(coords = c("geom_lat","geom_lon"), crs = WS_crs) %>%
  st_join(temps_trim,
          left = T,
          join = st_nearest_feature)


##-------------------------
#  Renaming of Columns for QRF model. QRF model requires specific columns, and column names. Refer to
#  "QRF_new_hab_cov_tbl.rds" for each model.
##-------------------------

load(here("Documentation/DASH_QRF_Metrics/QRF_new_hab_cov_tbl.rda"))

# First, hr scale....
QRF_hab_reach = hab_reach %>%
  rename(Site = site_nm,
         CU_Freq = cu_freq,
         FstNT_Freq = run_freq,
         FstTurb_Freq = fst_turb_freq,
         Sin = hr_sin_cl,
         WetBraid = hr_braidedness,
         FishCovLW = fish_cov_lwd,
         DpthThlwg_Avg = hr_thlwg_dpth_avg_m,
         PoolResidDpth = hr_avg_resid_pool_dpth_m,
         SubEstBldr = sub_est_bldr,
         SubEstCandBldr = sub_est_cbl_bldr,
         SubEstCbl = sub_est_cbl,
         SubEstGrvl = sub_est_gravl,
         SubEstSandFines = sub_est_sand_fines,
         LWFreq_Wet = lwd_wetted_freq,
         Lgth_Wet = hr_length_m,
         LON_DD = lon,
         LAT_DD = lat) %>%

  mutate(FishCovSome = 100 - fish_cov_none) %>%
  select(Site,
         hab_rch,
         avg_aug_temp,
         CU_Freq,
         FstNT_Freq,
         FstTurb_Freq,
         Sin,
         WetBraid,
         FishCovLW,
         FishCovSome,
         DpthThlwg_Avg,
         PoolResidDpth,
         SubEstBldr,
         SubEstCandBldr,
         SubEstCbl,
         SubEstGrvl,
         SubEstSandFines,
         LWFreq_Wet,
         Lgth_Wet,
         LON_DD,
         LAT_DD)

# Next, CU Scale....
QRF_chnl_unit = chnl_unit %>%
  mutate(FishCovSome = 100 - ttl_n__) %>%
  rename(Site = "site_nm",
         CU_Freq = "cu_freq",
         Sin = "hr_sin_cl",
         WetBraid = "hr_braidedness",
         DpthResid = "resid_depth_m",
         DpthThlwgExit = "thalweg_exit_depth_m",
         FishCovLW = "wdy_db_",
         SubEstGrvl = "g_2_64_",
         SubEstSandFines = "snd__2_",
         LON_DD = lon,
         LAT_DD = lat,
         Lgth_Wet = c_lngt_,
         Q = dschrg_cm) %>%
  select(Site,
         hab_rch,
         year,
         avg_aug_temp,
         CU_Freq,
         Sin,
         WetBraid,
         chnnl_nt_n,
         chnnl_nt_t,
         DpthResid,
         DpthThlwgExit,
         FishCovLW,
         FishCovSome,
         SubEstCandBldr,
         SubEstGrvl,
         SubEstSandFines,
         LON_DD,
         LAT_DD,
         Lgth_Wet,
         Q) %>%
  group_by(Site) %>%
  mutate(Q = if_else(is.na(Q), first(Q[!is.na(Q)]), Q))%>% #
  ungroup()

# Pull A few mets from QRF_hab_reach_data to add to QRF_chnl_unit_data
add_metrics <- QRF_hab_reach %>%
  select(Site, hab_rch, FstNT_Freq, FstTurb_Freq) %>%
  st_drop_geometry()

QRF_chnl_unit <- QRF_chnl_unit %>%
 left_join(add_metrics, by = c("Site", "hab_rch"))

##-------------------------
#  Now add area Wetted area to both QRF_hab_reach and QRF_chnl_unit. This is going to allow us to
#  calculate between per m and per m^2.
##-------------------------

example_cu_poly <- st_read(paste0(data_directory,"/spatial_files/channel_unit_polygons/cu_polygons.shp")) %>%
  mutate(hab_rch = as.numeric(hab_rch),
         cu_num  = as.numeric(cu_num))

# Make new DF formatted at HR Scale with areas at CU scale
add_area_df_cu <- example_cu_poly %>%
  select(site_name,hab_rch,cu_num,area) %>%
  st_drop_geometry()

# now HR
add_area_df_hr <- add_area_df_cu %>%
  group_by(site_name,hab_rch) %>%
  summarise(area = sum(area))

# Append Area Metrics to corresponding QRF data frames.

#habitat reach scale
QRF_hab_reach <- QRF_hab_reach %>%
  left_join(add_area_df_hr, by = c("Site" = "site_name", "hab_rch" = "hab_rch"))

#channel unit Scale
QRF_chnl_unit <- QRF_chnl_unit %>%
  left_join(add_area_df_cu, by = c("Site" = "site_name", "hab_rch" = "hab_rch", "chnnl_nt_n" = "cu_num"))


# Save DASH data prepped for QRF capacity model to "DASH_prepped_for_QRF" folder
write_csv(QRF_hab_reach, paste0(data_directory,"/7_qrf_ready/qrf_hab_reach.csv"))
write_csv(QRF_chnl_unit, paste0(data_directory,"/7_qrf_ready/qrf_chnl_unit.csv"))


### END SCRIPT

