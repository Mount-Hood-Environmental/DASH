# Authors: Bryce Oldemeyer, Bridger Bertram, Tulley Mackey
# Co-authors: Mike Ackerman and Kevin See
#
# Purpose: A script to join all of the centerlines, while adding
# information from the channel unit points.
#
# Initially created: December 8, 2020
# Last Modified: Bridger Bertram


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
library(here)

#-------------------------
# set directory to your spatial data
#-------------------------

# example_data
shapefile_path = here("data/example_data/spatial_files")

# project_data
#shapefile_path = here("data/project_data/spatial_files/")

#-------------------------
# read in centerlines
#-------------------------

# --- Old dash
# import centerlines using read_centerlines(). note, centerlines most be named "centerlines.shp"
#cl_sf = read_centerlines(path = shapefile_path, find_duplicates = T)

# --- new dash
cl_sf <- st_read(paste0(shapefile_path,"/centerlines/centerlines.shp"))

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
# Note: when naming channel unit points use this naming conventions "cu_points"
#-------------------------------------
#cu_pts_path = paste0(shapefile_path,"/channel_unit_points/")

# read in channel unit points from selected year
cu_pts = st_read(paste0(shapefile_path, "/channel_unit_points/cu_points.shp"))# %>%

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
         hab_rch) %>%
  distinct()

# are there any duplicate channel units in cu_pts_df?

# join cu_pts_df to centerlines
cl_sf %<>%
  left_join(cu_pts_df,
            by = c("site_name",
                   "year",
                   "cu_num")) %>%
  relocate(geometry,
           .after = last_col())

#-------------------------------------
# save merged centerlines
#-------------------------------------
st_write(cl_sf,
         dsn = paste0(shapefile_path, "/merged_centerlines/merged_centerlines.shp"),
         delete_dsn = F)
#-------------------------------------
# QC centerlines
#-------------------------------------
cl_qc = qc_centerline(cl_sf) # currently no errors found

#-------------------------
# read in otg data
#-------------------------

otg = readRDS(file = here("data/example_data/4_otg_rolled_cu/cu_imputed.rds"))  %>%
  mutate(year = as.numeric(str_extract(survey_start_date_time, "\\d{4}"))) %>%
  mutate(channel_segment_number = as.numeric(channel_segment_number)) %>%
  mutate(channel_unit_number = as.numeric(channel_unit_number)) %>%
  select(-channel_unit_notes)

#-------------------------
# join the merged_centerlines to the OTG data
#-------------------------

otg_sf = otg %>%
  left_join(cl_sf %>%
            select(-path_nm),
            by = c("site_name",
                   "year",
                   "channel_segment_number" = "seg_num",
                   "channel_unit_number" = "cu_num"))  %>%

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
         dsn = here("data/example_data/5_otg_spatial/otg_spatial.shp"),
         delete_dsn = T)

### END SCRIPT
