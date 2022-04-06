# Authors: Mike Ackerman
#
# Purpose: A script to join the OTG data to the compiled
# centerlines, making the entire dataset spatial
#
# Initially created: April 6, 2022
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(sf)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# read in otg & joined centerlines
#-------------------------
otg = readRDS(file = paste0(nas_prefix,
                            "main/data/habitat/DASH/OTG/prepped/dash_18to21_cu_imputed.rds"))

cl_sf = st_read(paste0(nas_prefix,
                       "main/data/habitat/DASH/centerlines/compiled/centerlines_all.shp"))

# START HERE
otg_tmp = otg %>%
  select(site_name,
         channel_segment_number,
         channel_unit_number,
         channel_unit_type) %>%
  unite(cu_id,
        site_name, channel_segment_number, channel_unit_number, channel_unit_type,
        remove = T)

cl_tmp = cl_sf %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(site_name,
         seg_num,
         cu_num,
         cu_type) %>%
  mutate(seg_num = str_pad(seg_num, 2, pad = "0"),
         cu_num = str_pad(cu_num, 3, pad = "0")) %>%
  unite(cu_id,
        site_name, seg_num, cu_num, cu_type,
        remove = T)

mismatches = anti_join(otg_tmp, cl_tmp)


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
