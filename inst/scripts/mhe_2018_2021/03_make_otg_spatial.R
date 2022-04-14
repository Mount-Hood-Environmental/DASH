# Authors: Mike Ackerman and Kevin See
#
# Purpose: A script to join all of the centerlines, while adding
# information from the channel unit points.
#
# Initially created: December 8, 2020
#   Last Modified: April 12, 2022
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

#-------------------------
# read in centerlines
#-------------------------
cl_path = paste0(nas_prefix,
                 "main/data/habitat/DASH/centerlines")

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
#-------------------------------------
cu_pts_path = paste0(nas_prefix,
                     "main/data/habitat/DASH/channel_units/compiled")

# read in channel unit points from 2018 - 2021
cu_pts = st_read(paste0(cu_pts_path, "/dash_cu_points.gpkg")) %>%
  mutate(seg_num = str_pad(seg_num, 2, pad = "0"),
         hab_rch = str_pad(hab_rch, 2, pad = "0"),
         cu_num = str_pad(cu_num, 3, pad = "0"))

# QC channel unit points
cu_pts_qc = qc_cu_points(cu_pts)

#-------------------------------------
# attach some data from channel unit points to centerlines sf
#-------------------------------------
cu_pts_df = cu_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(site_name,
         year,
         seg_num,
         cu_num,
         cu_type,
         hab_rch,
         fish_site,
         grts_id) %>%
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
         dsn = paste0(cl_path, "/compiled/centerlines_all.gpkg"),
         delete_dsn = T)

#-------------------------------------
# QC centerlines
#-------------------------------------
cl_qc = qc_centerline(cl_sf) # currently no errors found

#-------------------------
# read in otg data
#-------------------------
otg = readRDS(file = paste0(nas_prefix,
                            "main/data/habitat/DASH/OTG/prepped/dash_18to21_cu_imputed.rds")) %>%
  # extract year from survey_start_date_time
  mutate(year = as.numeric(str_extract(survey_start_date_time, "\\d{4}")))

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
         cu_sin_cl = cu_length_m / cu_strght_m)

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
         dsn = paste0(nas_prefix, "main/data/habitat/DASH/prepped/DASH_18to21.gpkg"),
         delete_dsn = T)

### END SCRIPT
