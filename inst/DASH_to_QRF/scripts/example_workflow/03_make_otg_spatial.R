# Authors: Mike Ackerman and Kevin See
#
# Purpose: A script to join all of the centerlines, while adding
# information from the channel unit points.
#
# Initially created: December 8, 2020
# Last Modified: April 24,2024 by Bridger Bertram to create a site specific workflow
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(stringr)
library(rlang)
library(purrr)
library(sf)
library(lwgeom)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------

if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

# Specify year and site
year = "2024"
watershed = "example"

#-------------------------
# read in centerlines
#-------------------------
cl_path = paste0(nas_prefix,
                 "main/data/habitat/DASH/centerlines/",year,"/",watershed,"/")

# import centerlines using read_centerlines()
cl_sf = read_centerlines(path = cl_path,
                         find_duplicates = T)

# plot centerlines
cl_p = cl_sf %>%
  ggplot() +
  geom_sf(aes(color = site_name),
          size = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(title = "DASH Centerlines")
cl_p

#-------------------------------------
# read in channel unit points (e.g., from Field Maps)
# Note: when naming channel unit points use this naming conventions "dash_cu_points_24_example"
#       replace "24" with year survey took place, and "example" with watershed
#-------------------------------------
cu_pts_path = paste0(nas_prefix,
                     "main/data/habitat/DASH/channel_units")

# read in channel unit points from selected year
cu_pts = st_read(paste0(cu_pts_path, "/dash_cu_points_",substr(year, nchar(year) - 1, nchar(year)),"_",watershed,".shp")) %>%
  mutate(seg_num = str_pad(seg_num, 2, pad = "0"),
         hab_rch = str_pad(hab_rch, 2, pad = "0"),
         cu_num = str_pad(cu_num, 3, pad = "0"))

# QC channel unit points
cu_pts_qc = qc_cu_points(cu_pts)

#-------------------------------------
# attach some data from channel unit points to centerlines sf
#-------------------------------------

# Note: The renaming of entries in site_name column is specific for lemhi-hayden complex.
#       Be sure to change this to make site_name from cu_pts_df to match cl_sf

cu_pts_df = cu_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(site_name,
         year,
         seg_num,
         cu_num,
         cu_type,
         hab_rch) %>%
  mutate(site_name = ifelse(site_name == 'Pre Restoration', 'Hayden_Pre Restoration', site_name),
         site_name = ifelse(site_name == 'Split River Ranch', 'Split_River', site_name),
         site_name = ifelse(site_name == 'Fish Gamer', 'Fish_Gamer', site_name)) %>%
  distinct()

# are there any duplicate channel units in cu_pts_df?
dup_pt_cus = cu_pts_df %>%
  unite(col = cu_id,
        site_name, year, cu_num,
        remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)])
dup_pt_cus # no duplicate channel units (anymore)

# join cu_pts to centerlines
cl_sf %<>%
  left_join(cu_pts_df,
            by = c("site_name",
                   "year",
                   "cu_num")) %>%
  relocate(geometry,
           .after = last_col())

#-------------------------------------
# save compiled centerlines
#-------------------------------------
st_write(cl_sf,
         dsn = paste0(cl_path, "compiled/centerlines_",year,".shp"),
         delete_dsn = TRUE)
#-------------------------------------
# QC centerlines
#-------------------------------------
cl_qc = qc_centerline(cl_sf) # currently no errors found

#-------------------------
# read in otg data
#-------------------------

# Note: The renaming of entries in site_name column is specific for lemhi-hayden complex.
#       Be sure to change this to make site_name from OTG to match cl_sf

otg = readRDS(file = paste0(nas_prefix,
              "main/data/habitat/DASH/OTG/",year,"/",watershed,"/3_prepped_otg/dash_",year,"_",watershed,"_cu_imputed.rds")) %>%

  # extract year from survey_start_date_time
  mutate(year = as.numeric(str_extract(survey_start_date_time, "\\d{4}"))) %>%
  mutate(site_name = ifelse(site_name == 'Pre Restoration', 'Hayden_Pre Restoration', site_name),
         site_name = ifelse(site_name == 'Split River Ranch', 'Split_River', site_name),
         site_name = ifelse(site_name == 'Fish Gamer', 'Fish_Gamer', site_name)) %>%
  select(-channel_unit_notes)

#-------------------------
# join the centerlines to the OTG data
#-------------------------
otg_sf = otg %>%
  left_join(cl_sf %>%
              select(-path_nm),
            by = c("site_name",
                   "year",
                   "channel_segment_number" = "seg_num",
                   "channel_unit_number" = "cu_num")) %>%
  relocate(geometry,
           .after = last_col()) %>%

  # convert to sf object
  st_as_sf() %>%
  # convert to EPSG: 32612 = WGS 84/UTM zone 12N, which allows us to calculate some geometries
  st_transform(crs = 32612) %>%
  mutate(geo_type = st_geometry_type(geometry),
         # length of centerline
         cu_length_m = as.numeric(st_length(geometry)),
         # straight line distance of centerline
         cu_strght_m = as.numeric(map2(st_startpoint(geometry),
                                       st_endpoint(geometry),
                                       st_distance)),
         cu_sin_cl = cu_length_m / cu_strght_m) %>%
  mutate(
    midpoint = st_line_sample(geometry, sample = 0.5),
    midpoint_geo = st_transform(midpoint, crs = 32612),
    lat = st_coordinates(midpoint_geo)[,2],
    lon = st_coordinates(midpoint_geo)[,1]
  ) %>%
  select(-midpoint, -midpoint_geo)

# do channel unit types match btw otg and channel unit points?
cu_match = otg_sf %>%
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
otg_sf %<>%
  # remove cu_type from cu points
  select(-cu_type)

# write spatial otg as geodatabase
st_write(otg_sf,
         dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_",year,"_",watershed,".shp"),
         delete_dsn = T)

### END SCRIPT
