# Authors: Kevin See
#
# Purpose: A script to join OTG data to
# spatial centerlines, making the OTG channel
# unit data spatial
#
# Created: December 8, 2020
# Last Modified:
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

#-------------------------
# read in centerlines
#-------------------------
cl_path = paste0(nas_prefix,
                 "/data/habitat/DASH/centerlines/2019")

cl_list = list.files(path = cl_path,
                     pattern = "^cl_join.shp$",
                     recursive = T) %>%
  paste(cl_path, ., sep = "/") %>%
  as.list() %>%
  map(.f = function(x) {
    read_sf(x) %>%
      dplyr::mutate(path_nm = stringr::str_remove(x, cl_path)) %>%
      select(path_nm,
             everything())
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
  mutate(file_row_id = 1:n()) %>%
  select(path_nm,
         file_row_id,
         everything())

rm(cl_list)

# do a little QC
cl_qc = qc_centerline(cl_sf)

# save the file as a csv
# add a local object ID to depict the row number within each file
cl_qc %>%
  inner_join(cl_sf %>%
               group_by(path_nm) %>%
               mutate(object_id = 1:n()) %>%
               as_tibble() %>%
               select(path_nm, file_row_id, object_id)) %>%
  select(path_nm, file_row_id, object_id, everything()) %>%
  arrange(path_nm, file_row_id, error_message) %>%
  write_csv(paste0(cl_path,
                 "/QC_centerlines_",
                 format(Sys.Date(), format = "%Y%m%d"),
                 ".csv"))


cl_qc %>%
  select(file_row_id) %>%
  distinct() %>%
  left_join(cl_sf)

#-------------------------
# load QC'd OTG data
#-------------------------
otg_path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/2019")

# list of otg_raw files in otg_path
otg_list = list.files(path = otg_path,
                           pattern = "^otg_qcd.rda$",
                           recursive = T) %>%
  paste(otg_path, ., sep = "/") %>%
  as.list() %>%
  map(.f = function(x) {
    load(x) %>%
      get() %>%
      map(clean_names)
  })

for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg_all = otg_list[[1]]
  } else {
    otg_all = suppressMessages(purrr::map2(otg_all,
                                               otg_list[[i]],
                                               dplyr::full_join))
  }
}

# clean up
rm(otg_list)

# clean up some site names to match centerline file
otg_all$survey = otg_all$survey %>%
  rename(Site_ID = site_name) %>%
  mutate(Site_ID = recode(Site_ID,
                          "Lowerlemhi3_2019" = "LowerLemhi3_2019"))

#-------------------------
# join the OTG data
#-------------------------
cl_sf %>%
  rename(cl_path = path_nm) %>%
  anti_join(otg_all$survey) %>%
  tabyl(Site_ID)

# these site names are shared between the centerlines and the OTG data
unique(cl_sf$Site_ID)[unique(cl_sf$Site_ID) %in% unique(otg_all$survey$Site_ID)]
# these sites are in the centerline data but not the OTG
unique(cl_sf$Site_ID)[!unique(cl_sf$Site_ID) %in% unique(otg_all$survey$Site_ID)]
# these sites are in the OTG data but not the centerlines
unique(otg_all$survey$Site_ID)[!unique(otg_all$survey$Site_ID) %in% unique(cl_sf$Site_ID)]

cl_sf %>%
  inner_join(otg_all$cu %>%
               select(-c(creation_date:editor),
                      -path_nm,
                      -object_id) %>%
               rename(CU_Number = channel_unit_number,
                      Seg_Number = channel_segment_number) %>%
               left_join(otg_all$survey %>%
                           select(parent_global_id = global_id,
                                  Site_ID,
                                  survey_date))) %>%
  # filter(Site_ID == "Barton_2019",
  #        CU_Number %in% 15:17)
  as_tibble() %>%
  tabyl(CU_Type, channel_unit_type)

cl_sf %>%
  inner_join(otg_all$cu %>%
               select(-c(creation_date:editor),
                      -path_nm,
                      -object_id) %>%
               rename(CU_Number = channel_unit_number,
                      Seg_Number = channel_segment_number) %>%
               left_join(otg_all$survey %>%
                           select(parent_global_id = global_id,
                                  Site_ID,
                                  survey_date))) %>%
  # filter(Site_ID == "BigTimber1_2019") %>%
  filter(grepl('Hayden', Site_ID)) %>%
  ggplot() +
  geom_sf(aes(color = CU_Number)) +
  scale_color_viridis_c() +
  theme_bw()



otg_all$wood %>%
  left_join(otg_all$cu %>%
              select(matches("global_id"),
                     channel_unit_type,
                     channel_unit_number,
                     channel_segment_number) %>%
              left_join(otg_all$survey %>%
                          select(global_id,
                                 site_name,
                                 survey_date),
                        by = c("parent_global_id" = "global_id")),
            by = c("parent_global_id" = "global_id")) %>%
  janitor::tabyl(site_name)
