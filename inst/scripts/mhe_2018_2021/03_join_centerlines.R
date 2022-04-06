# Authors: Mike Ackerman and Kevin See
#
# Purpose: A script to join all of the centerlines, while adding
# information from the channel unit points.
#
# Initially created: December 8, 2020
#   Last Modified: April 6, 2022
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

# get path name to all centerlines.shp files
cl_files = list.files(path = cl_path,
                      pattern = "centerlines.shp$",
                      recursive = TRUE) %>%
  as.list() %>%
  rlang::set_names(nm = function(x) str_remove(x, "/centerlines.shp$")) %>%
  map(.f = function(x) {
    paste(cl_path, x, sep = "/")
  })

# the columns we're interested in from the centerlines.shp files
col_nms = c("path_nm",
            "site_name",
            "year",
            "cu_num",
            "geometry")

# read in all cl_files
cl_list = cl_files %>%
  map(.f = function(x) {
    read_sf(x) %>%
      dplyr::mutate(path_nm = stringr::str_remove(x, cl_path)) %>%
      janitor::clean_names() %>%
      select(path_nm,
             everything())
  }) %>%
  map(.f = function(x) {
    if(sum(!col_nms %in% names(x)) > 0) {
      for(col_nm in col_nms[!col_nms %in% names(x)]) {
        x[,col_nm] = NA_character_
      }
    }
    x %>%
      select(any_of(col_nms))
  })

# merge cl_list into a single sf object
cl_sf = map_df(cl_list,
                .f = identity) %>%
  # add an object_id column
  mutate(object_id = 1:n()) %>%
  select(object_id,
         path_nm,
         everything())

# a little cleaning
rm(cl_files, cl_list, col_nms)

# plot centerlines
cl_p = cl_sf %>%
  ggplot() +
  geom_sf() +
  theme_classic()
cl_p

# any duplicate channel units within the centerlines
dup_cl_cus = cl_sf %>%
  unite(col = cu_id,
        site_name,
        year,
        cu_num,
        remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  st_drop_geometry() %>%
  pull(cu_id)

dup_cl_cus # no duplicate channel units (anymore)

#-------------------------------------
# read in DASH Field Maps
#-------------------------------------
cu_points_path = paste0(nas_prefix,
                        "main/data/habitat/DASH/channel_units/compiled")

# read in channel unit points from 2018 - 2021
cu_pts = st_read(paste0(cu_points_path, "/dash_cu_points.shp"))

# are there any channel units in multiple habitat reaches?
cu_hr_mismatch = cu_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(strm_nm,
         site_nm,
         year,
         cu_num,
         cu_type,
         hab_rch) %>%
  distinct() %>%
  unite(cu_id,
        strm_nm, site_nm, year, cu_num,
        remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  pull(cu_id) %>%
  unique()
length(cu_hr_mismatch) # these are fine, not an issue

# which sites are in collector points, but not in centerlines?
unique(cu_pts$site_nm)[! unique(cu_pts$site_nm) %in% unique(cl_sf$site_name)]

# which sites are in centerlines, but not collector points?
unique(cl_sf$site_name[! unique(cl_sf$site_name) %in% unique(cu_pts$site_nm)])

# add some information from collector points to centerlines spatial file
cu_pts_df = cu_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(strm_nm,
         site_nm,
         year,
         seg_num,
         cu_num,
         cu_type,
         hab_rch,
         fish_st,
         grts_id) %>%
  distinct()

# are there any duplicate channel units in the cu pts?
dup_pt_cus = cu_pts_df %>%
  unite(col = cu_id,
        site_nm, year, cu_num,
        remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  write_csv(file = paste0(cu_points_path,
                          "/duplicate_cu_pts.csv"))
dup_pt_cus # no duplicate channel units (anymore)

# join cu_pts to centerlines
cl_sf %<>%
  left_join(cu_pts_df,
            by = c("year",
                   "site_name" = "site_nm",
                   "cu_num")) %>%
  relocate(geometry,
           .after = last_col())

#-------------------------------------
# save raw compiled centerlines
#-------------------------------------

# as shapefile
st_write(cl_sf,
         paste0(cl_path,
                "/compiled/centerlines_all.shp"),
         append = F)

# as geodatabase
st_write(cl_sf,
         dsn = paste0(cl_path, "/compiled/centerlines_all.gpkg"),
         delete_dsn = T)

#-------------------------------------
# QC centerlines
#-------------------------------------
cl_qc = qc_centerline(cl_sf) # currently no errors found

### END SCRIPT
