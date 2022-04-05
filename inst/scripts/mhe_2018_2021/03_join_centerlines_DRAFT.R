# Authors: Mike Ackerman and Kevin See
#
# Purpose: A script to join the OTG data to the spatial
# centerlines, making the OTG channel unit data spatial.
#
# Initially created: December 8, 2020
#   Modified: March 29, 2022
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
dup_cus = cl_sf %>%
  unite(col = cu_id,
        site_name,
        year,
        cu_num,
        remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  st_drop_geometry() %>%
  pull(cu_id)

dup_cus # no duplicate channel units (anymore)

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
st_write(cl_sf,
         dsn = paste0(cl_path, "/compiled/centerlines_raw.gpkg"),
         delete_dsn = T)

#-------------------------------------
# QC centerlines
#-------------------------------------
cl_qc = qc_centerline(cl_sf) # currently no errors found



### START HERE
# clean up some site names to match centerline file
cu_df = cu_df %>%
  rename(Site_ID = site_name) %>%
  mutate(Site_ID = recode(Site_ID,
                          "Lowerlemhi3" = "LowerLemhi3")) %>%
  mutate(Site_ID = paste(Site_ID, lubridate::year(survey_date), sep = "_"))

#-------------------------
# join the OTG data
#-------------------------
# these site names are shared between the centerlines and the OTG data
unique(cl_sf$Site_ID)[unique(cl_sf$Site_ID) %in% unique(cu_df$Site_ID)]
# these sites are in the centerline data but not the OTG
unique(cl_sf$Site_ID)[!unique(cl_sf$Site_ID) %in% unique(cu_df$Site_ID)]
# these sites are in the OTG data but not the centerlines
unique(cu_df$Site_ID)[!unique(cu_df$Site_ID) %in% unique(cl_sf$Site_ID)]

cl_sf %>%
  rename(cl_path = path_nm) %>%
  st_drop_geometry() %>%
  anti_join(cu_df) %>%
  tabyl(Site_ID)

cu_df %>%
  anti_join(cl_sf %>%
  # inner_join(cl_sf %>%
              rename(cl_path = path_nm) %>%
              st_drop_geometry()) %>%
  tabyl(Site_ID)


cu_spatial = cl_sf %>%
  select(-path_nm) %>%
  inner_join(cu_df %>%
               select(-matches('global_id'),
                      -path_nm) %>%
               rename(CU_Number = channel_unit_number,
                      Seg_Number = channel_segment_number) %>%
               mutate(across(c(CU_Number,
                               Seg_Number),
                             as.numeric)))

# any duplicated channel units?
cu_spatial %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  arrange(cu_id) %>%
  select(Site_ID,
         Seg_Number,
         CU_Number) %>%
  st_drop_geometry() %>%
  distinct() %>%
  left_join(cl_sf)

cl_sf %>%
  # filter(Site_ID == "LowerLemhi3_2019",
  #        CU_Number == 39) %>%
  # filter(Site_ID == "BigTimber1_2019",
  #        CU_Number %in% c(11, 75)) %>%
  filter(Site_ID == "Hayden1_2019",
         CU_Number %in% c(6)) %>%
  select(Site_ID, CU_Number, path_nm)



cl_sf %>%
  filter(!is.na(Site_ID)) %>%
  mutate(across(c(Seg_Number, CU_Number),
                str_pad,
                width = 3,
                pad = "0")) %>%
  tidyr::unite("cu_id",
               Site_ID, Seg_Number, CU_Number,
               remove = F) %>%
  filter(cu_id %in% cu_id[duplicated(cu_id)]) %>%
  arrange(cu_id) %>%
  select(path_nm, cu_id)

cu_spatial %>%
  filter(grepl('Hayden', Site_ID)) %>%
  ggplot() +
  geom_sf(aes(color = CU_Number)) +
  scale_color_viridis_c() +
  theme_bw()
