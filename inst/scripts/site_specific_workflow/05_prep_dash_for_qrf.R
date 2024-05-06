# Author: Bryce Oldemeyer, Tulley Mackey
# Purpose: Estimate habitat capacity for DASH surveyed sites at the Lemhi Hayden complex
# Created: 1/30/2024
# Last Modified: 1/30/2024
# Modified by: Bryce Oldemeyer, Mark Roes, and Mike Ackerman, Bridger Bertram
#
#
# clear environment
rm(list = ls())

# load needed libraries

library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)
library(readxl)
library(here)
library(patchwork)
library(ggforce)

#-------------------------
# set NAS prefix (depending on operating system), set year and watershed of your site,
# and read in Habitat data.
#-------------------------

if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }

# Specify year and site
year = "2024"
watershed = "example"


#Read in Channel Unit and Habitat Reach data...
hab_reach_data = readRDS(paste0(nas_prefix,"main/data/habitat/DASH/prepped/dash_hr_",year,"_",watershed,".rds"))
chnl_unit_data = readRDS(paste0(nas_prefix,"main/data/habitat/DASH/prepped/dash_cu_",year,"_",watershed,".rds"))

##-------------------------
#  Append Temperature Data From reach_200 file
#
#  Note: Project CRS, and watershed boundary are specific to Lemhi-Hayden complex.
#        Will need to modify this step for other sites outside of Lemhi WS Basin.
#        All upper Salmon Watershed boundaries can be found on the NAS
##-------------------------

WS_crs = st_crs(4326) # WGS84

#specify watershed boundary.
ws_bound = st_read(paste0(nas_prefix,"main/data/habitat/watershed_boundaries/Lemhi_WS.gpkg")) %>%
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
chnl_unit_data <- chnl_unit_data %>%
  st_as_sf(coords = c("lat","lon")) %>%
  st_transform(crs = WS_crs) %>%
  st_join(temps_trim,
          left = T,
          join = st_nearest_feature)

# add temps_trim to hab_reach_data
hab_reach_data <- hab_reach_data %>%
  mutate(geom_lat = lat,
         geom_lon = lon) %>%
  st_as_sf(coords = c("geom_lat","geom_lon"), crs = WS_crs) %>%
  st_join(temps_trim,
          left = T,
          join = st_nearest_feature)


##-------------------------
#  Renaming of Columns for QRF model. QRF model requires specific columns, and column names. Refer to
#  "QRF_new_hab_cov_tbl.rds" for each model.
#
#  Note: MHE folks, maybe we can standardize column names when collecting dash survey data to potentially skip this step
#        or make it more efficient?
##-------------------------

load(here("inst/QRF_new_hab_cov_tbl.rda"))


# First, hr scale....

QRF_hab_reach_data = hab_reach_data %>%
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

QRF_chnl_unit_data = chnl_unit_data %>%
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
add_metrics <- QRF_hab_reach_data  %>%
  select(Site, hab_rch, FstNT_Freq, FstTurb_Freq) %>%
  st_drop_geometry()

QRF_chnl_unit_data <- QRF_chnl_unit_data %>%
 left_join(add_metrics, by = c("Site", "hab_rch"))

##-------------------------
#  Now add area Wetted area to both QRF_hab_reach and QRF_chnl_unit. This is going to allow us to
#  calculate between per m and per m^2.
#
#  Note: For this step, someone is going to have to draw some polygons delinating the channel of the site at the
#        channel unit scale. (From there is is easy to roll up to HR scale). Save the polygons somewhere handy.
##-------------------------


# These Shape files are specific to Lemhi-Hayden complex.
# Pull in areas from shape files
hayden_shp <- st_read("S:/main/data/habitat/DASH/cu_polygons/2022/Hayden_Pre Restoration.shp")
split_shp <- st_read("S:/main/data/habitat/DASH/cu_polygons/2022/Split_River.shp")
fish_gamer_shp <- st_read("S:/main/data/habitat/DASH/cu_polygons/2022/Fish_Gamer.shp")


# Make new DF formatted at HR Scale with areas at CU scale
add_area_df_cu <- bind_rows(hayden_shp,split_shp,fish_gamer_shp) %>%
  select(site_name,hab_rch,cu_num,area) %>%
  rename("chnnl_nt_n" = "cu_num",
         "Site" = "site_name") %>%
  st_drop_geometry()

# now HR
add_area_df_hr <- add_area_df_cu %>%
  group_by(Site,hab_rch) %>%
  summarise(area = sum(area))

# Append Area Metrics to corresponding QRF data frames.

#habitat reach scale
QRF_hab_reach_data <- QRF_hab_reach_data %>%
  left_join(add_area_df_hr, by = c("Site" = "Site", "hab_rch" = "hab_rch"))

#channel unit Scale
QRF_chnl_unit_data <- QRF_chnl_unit_data %>%
  left_join(add_area_df_cu, by = c("Site" = "Site", "hab_rch" = "hab_rch", "chnnl_nt_n" = "chnnl_nt_n"))


# Save DASH data prepped for QRF capacity model to "DASH_prepped_for_QRF" folder
write_csv(QRF_hab_reach_data, paste0(nas_prefix,"main/data/habitat/DASH/DASH_prepped_for_QRF/",year,"/",watershed,"/habitat_reach_scale.csv"))
write_csv(QRF_chnl_unit_data, paste0(nas_prefix,"main/data/habitat/DASH/DASH_prepped_for_QRF/",year,"/",watershed,"/channel_unit_scale.csv"))


### END SCRIPT

