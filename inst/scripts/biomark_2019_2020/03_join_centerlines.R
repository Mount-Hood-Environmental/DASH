# Authors: Kevin See
#
# Purpose: A script to join OTG data to
# spatial centerlines, making the OTG channel
# unit data spatial
#
# Created: December 8, 2020
# Last Modified: July 9, 2021
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(sf)
#library(DASH)
devtools::load_all()

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

# for using a local copy
nas_prefix = "~/Desktop"

#-------------------------
# read in centerlines
#-------------------------
# cl_path = paste0(nas_prefix,
#                  "/data/habitat/DASH/centerlines/2019")

cl_path = paste0(nas_prefix,
                 "/data/habitat/DASH/centerlines")

cl_list = list.files(path = cl_path,
                     pattern = "centerlines.shp$",
                     recursive = T) %>%
  as.list() %>%
  rlang::set_names(nm = function(x) str_remove(x, "/centerlines.shp$")) %>%
  map(.f = function(x) {
    paste(cl_path, x, sep = "/")
  }) %>%
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


cl_sf = NULL
for(i in 1:length(cl_list)) {
  if(i == 1) {
    cl_sf = cl_list[[i]]
  } else {
    cl_sf = rbind(cl_sf,
                  cl_list[[i]])
  }
}
cl_sf = cl_sf %>%
  mutate(object_id = 1:n()) %>%
  select(object_id,
         path_nm,
         everything())

rm(cl_list)

#-------------------------------------
# determine what habitat reach each channel unit is part of (and type of channel unit)
hr_path = paste0(nas_prefix,
                 "/data/habitat/DASH/habitat_reaches")

hab_rch = list.files(path = hr_path,
                     pattern = "_HR_",
                     recursive = T) %>%
  as.list() %>%
  rlang::set_names(nm = function(x) {
    str_split(x, "/", simplify = T)[,3] %>%
      str_remove(".csv$")
      }) %>%
  map(.f = function(x) {
    paste(hr_path, x, sep = "/")
  }) %>%
  # map(.f = read_csv)
  map_df(.id = "ID",
         .f = function(x) {
           read_csv(x) %>%
             mutate(across(c(CU_Number, Reach_Num),
                           as.numeric))
           }) %>%
  mutate(year = str_extract(Site_ID, "_[:digit:]+"),
         year = str_remove(year, "^_"),
         site_num = str_extract(Site_ID, "[:digit:]+_"),
         site_num = str_remove(site_num, "_"),
         site_nm = str_extract(Site_ID, "[:alpha:]+")) %>%
  mutate(across(c(year, site_num),
                as.numeric)) %>%
  mutate(site_nm = forcats::fct_relabel(site_nm,
                               .fun = make_clean_names,
                               case = "title"),
         site_nm = forcats::fct_recode(site_nm,
                                        "EF Bohannon" = "Ef Bohannon")) %>%
  mutate(site_name = paste(site_nm, site_num, sep = " "),
         site_name = str_remove(site_name, " NA$")) %>%
  select(-site_num, -site_nm)

unique(cl_sf$site_name)[!unique(cl_sf$site_name) %in% unique(hab_rch$site_name)]
unique(hab_rch$site_name)[!unique(hab_rch$site_name) %in% unique(cl_sf$site_name)]

cl_sf %>%
  left_join(hab_rch %>%
              select(site_name,
                     year,
                     cu_num = CU_Number,
                     seg_num = Seg_Number,
                     reach_num = Reach_Num) %>%
              distinct(),
            by = c("year", "site_name", "cu_num")) %>%
  filter(object_id %in% object_id[duplicated(object_id)])

#-------------------------------------
# save raw compiled centerlines
st_write(cl_sf,
         dsn = paste0(cl_path, "/compiled/centerlines_raw.gpkg"))

#-------------------------------------
# do a little QC
#-------------------------------------
cl_qc = qc_centerline(cl_sf)

# save the file as a csv
cl_qc %>%
  write_csv(paste0(cl_path,
                 "/compiled/QC_centerlines_",
                 format(Sys.Date(), format = "%Y%m%d"),
                 ".csv"))

# examine some of the channel units with issues
cl_qc %>%
  arrange(path_nm, object_id,
          error_message) %>%
  left_join(cl_sf %>%
              st_drop_geometry())

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
