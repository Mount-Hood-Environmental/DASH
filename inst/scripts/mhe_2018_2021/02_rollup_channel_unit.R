# Authors: Mike Ackerman
#
# Purpose: rollup OTG data to channel unit scale
#
# Created: March 4, 2022
# Last Modified: March 23, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load all OTG data
#-------------------------
otg_path = "Public Data/data/habitat/DASH/OTG/"
otg = readRDS(file = paste0(nas_prefix,
                            otg_path,
                            "prepped/otg_all_18to21.rds"))

# remove the qc_results data frame from otg
otg$qc_results = NULL

# let's run clean_names on everything before moving on... This is additionally done in otg_to_cu()
otg$survey %<>%
  janitor::clean_names()
otg$cu %<>%
  janitor::clean_names()
otg$wood %<>%
  janitor::clean_names()
otg$jam %<>%
  janitor::clean_names()
otg$undercut %<>%
  janitor::clean_names()
otg$discharge %<>%
  janitor::clean_names()

#-------------------------
# roll up OTG data to CU scale, no data imputation
#-------------------------
otg_cu = otg_to_cu(survey_df = otg$survey,
                   cu_df = otg$cu,
                   wood_df = otg$wood,
                   jam_df = otg$jam,
                   undercut_df = otg$undercut,
                   discharge_df = otg$discharge,
                   fix_nas = FALSE)

# save non-imputed, prepped data
write_csv(otg_cu,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/dash_18to21_cu_no_impute.csv"))

write_rds(otg_cu,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/dash_18to21_cu_no_impute.rds"))

#-------------------------
# PRIMARY IMPUTE: Explore imputation of variables in wood_df, jam_df, and undercut_df
# prior to executing in otg_to_cu()
#-------------------------

################################
# WOOD: length_m, diameter_m
wood_imp = otg$wood %>%
  select(path_nm,
         length_m,
         diameter_m) %>%
  mutate(path_nm = str_extract(path_nm, "[^/]+")) %>%
  mutate(impute_obs = if_else(is.na(length_m) | is.na(diameter_m), 1, 0) %>%
           as.factor())

# NA values before impute
wood_imp %>%
  filter(impute_obs == 1)

# impute NAs, using defaults
wood_imp = impute_missing_values(wood_imp,
                                 col_nm_vec = c("length_m",
                                                "diameter_m"),
                                 method = "randomForestSRC",
                                 my_seed = 5,
                                 ntree = 1000,
                                 nk = 4)

# values after impute
wood_imp %>%
  filter(impute_obs == 1)

# plot w/ imputed values
wood_imp %>%
  ggplot(aes(x = length_m,
             y = diameter_m,
             color = impute_obs)) +
  geom_point() +
  scale_color_manual(values = c("0" = "black",
                                "1" = "red")) +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~path_nm) +
  labs(x = "Wood Piece Length (m)",
       y = "Wood Piece Diameter (m)")

# For those w/ a "sufficient" sample size, there does seem to be a slightly pos+ relationship btw length and
# diameter, which makes sense. In some cases with lesser sample size, not always the case. However, probably
# makes sense to just use length_m and diameter_m for imputation (i.e., use all data, no need to also use
# site). There's likely always a somewhat consistent relationship btw wood length and diameter for LWD,
# regardless of species of "wood" and site. Interesting conversation on plant evolution there! Let's impute
# length_m and diameter_m themselves, using default values. There doesn't seem to be any other vars in otg_cu
# that make sense to use.

#######################################
# JAM: estimated_number_of_pieces, length_m, width_m, height_m
jam_imp = otg$jam %>%
  select(path_nm,
         estimated_number_of_pieces,
         length_m,
         width_m,
         height_m) %>%
  mutate(path_nm = str_extract(path_nm, "[^/]+")) %>%
  mutate(impute_obs = ifelse(is.na(estimated_number_of_pieces) |
                               is.na(length_m) |
                               is.na(width_m) |
                               is.na(height_m),
                             1, 0) %>%
           as.factor())

# NA values before impute
jam_imp %>%
  filter(impute_obs == 1)

# impute NAs, using defaults
jam_imp = impute_missing_values(jam_imp,
                                col_nm_vec = c("estimated_number_of_pieces",
                                               "length_m",
                                               "width_m",
                                               "height_m"),
                                method = "randomForestSRC",
                                my_seed = 5,
                                ntree = 1000,
                                nk = 4)

# values after impute
jam_imp %>%
  filter(impute_obs == 1)

#######################################
# UNDERCUT: length_m, width_25_percent_m, width_50_percent_m, width_75_percent_m
und_imp = otg$undercut %>%
  select(path_nm,
         parent_global_id,
         location,
         length_m,
         width_25_percent_m,
         width_50_percent_m,
         width_75_percent_m) %>%
  mutate(impute_obs = ifelse(is.na(length_m) |
                               is.na(width_25_percent_m) |
                               is.na(width_50_percent_m) |
                               is.na(width_75_percent_m),
                             1, 0) %>%
           as.factor())

# before imputation
und_imp %>%
  filter(impute_obs == 1) # no missing values

# clean up the workspace a tad
rm(wood_imp,
   und_imp)

#-------------------------
# roll up OTG data to CU scale, add primary data imputation
#-------------------------
otg_cu_imp = otg_to_cu(survey_df = otg$survey,
                       cu_df = otg$cu,
                       wood_df = otg$wood,
                       jam_df = otg$jam,
                       undercut_df = otg$undercut,
                       discharge_df = otg$discharge,
                       fix_nas = TRUE)

#-------------------------
# write rolled up CU data, with "primary" imputation complete
#-------------------------
# save non-imputed, prepped data
write_csv(otg_cu_imp,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/dash_18to21_cu_imputed.csv"))

write_rds(otg_cu_imp,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/dash_18to21_cu_imputed.rds"))

#-------------------------
# REMAINING ERRORS
#-------------------------

# The following errors still need to be addressed, largely in the 2019/2020 data


# 1. Missing ocular estimates - sand_fines_2mm_percent:boulder_256mm_percent
#       - Does it make sense to use information from pebble counts for riffles (where available)
# 2. Missing SSC widths: width_1_m:width_5_m

####################
# END 3/23/2022
####################

#######################################
# FISH COVER: overhanging_percent:total_no_cover_percent
miss_cover = otg_cu_imp %>%
  select(path_nm,
         global_id,
         channel_segment_number,
         channel_unit_number,
         channel_unit_type,
         overhanging_percent:total_no_cover_percent) %>%
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(miss_obs = case_when(
    is.na(overhanging_percent) ~ "1",
    is.na(aquatic_vegetation_percent) ~ "1",
    is.na(woody_debris_percent) ~ "1",
    is.na(artificial_percent) ~ "1",
    is.na(total_no_cover_percent) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
    )

# View records w/ missing values
miss_cover %>%
  filter(miss_obs == 1)

# write records with missing values
miss_cover %>%
  filter(miss_obs == 1) %>%
  write_csv(paste0(nas_prefix,
                   otg_path,
                   "prepped/missing_cover.csv"))

#######################################
# OCULAR: sand_fines_2mm_percent:boulder_256_percent
ocular_imp = otg_cu_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_number,
         channel_unit_type,
         sand_fines_2mm_percent:boulder_256mm_percent) %>%
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(impute_obs = case_when(
    is.na(sand_fines_2mm_percent) ~ "1",
    is.na(gravel_2_64mm_percent) ~ "1",
    is.na(cobble_64_256mm_percent) ~ "1",
    is.na(boulder_256mm_percent) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
  ) %>%
  filter(impute_obs == 1)

write_csv(ocular_imp,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/missing_ocular.csv"))

#######################################
# SIDE CHANNEL WIDTHS: width_1_m:width_5_m
sc_width_imp = otg_cu_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_number,
         channel_unit_type,
         width_1_m:width_5_m) %>%
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  filter(segment_type == "side_channel",
         channel_unit_type == "SSC") %>%
  mutate(impute_obs = case_when(
    is.na(width_1_m) ~ "1",
    is.na(width_2_m) ~ "1",
    is.na(width_3_m) ~ "1",
    is.na(width_4_m) ~ "1",
    is.na(width_5_m) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
  ) %>%
  filter(impute_obs == 1)

write_csv(sc_width_imp,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/missing_side_channel_widths.csv"))

#######################################
# maximum_depth_m, thalweg_exit_depth_m
cu_dpth_imp = otg_cu_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_type,
         maximum_depth_m,
         thalweg_exit_depth_m) %>%
  # assign each CU as either mainstem or side channel
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(impute_obs = case_when(
    is.na(maximum_depth_m) ~ "1",
    is.na(thalweg_exit_depth_m) & channel_unit_type != "OCA" ~ "1",
    TRUE ~ "0") %>%
      as.factor()
  ) %>%
  filter(impute_obs == 1)

miss_depth = tabyl(cu_dpth_imp, path_nm, channel_unit_type)
write_csv(miss_depth,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/missing_depths.csv"))

