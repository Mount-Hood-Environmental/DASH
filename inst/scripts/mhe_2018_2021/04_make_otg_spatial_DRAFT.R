
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
