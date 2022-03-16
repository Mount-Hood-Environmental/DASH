# Authors: Mike Ackerman
#
# Purpose: rollup OTG data to channel unit scale
#
# Created: March 4, 2022
# Last Modified: March 14, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load all OTG data
#-------------------------
otg = readRDS(file = paste0(nas_prefix,
                            "Public Data/data/habitat/DASH/OTG/prepped/otg_all_18to21.rds"))
otg = readRDS(file = "C:/Users/mikea/Downloads/otg_all_18to21.rds")

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
write_csv(cu_rllup,
          paste0(otg_path,
                 "/prepped/dash_1920_cu_no_impute.csv"))

write_rds(cu_rllup,
          paste0(otg_path,
                 "/prepped/dash_1920_cu_no_impute.rds"))

#-------------------------
# PRIMARY IMPUTE: Explore imputation of variables in wood_df, jam_df, and undercut_df
# prior to otg_to_cu()
#-------------------------

###############################
# Wood: length_m, diameter_m
wood_imp = otg$wood %>%
  select(path_nm,
         length_m,
         diameter_m) %>%
  mutate(path_nm = str_extract(path_nm, "[^_]+")) %>%
  mutate(impute_obs = ifelse(is.na(length_m) | is.na(diameter_m), 1, 0) %>%
           as.factor())

# before impute
wood_imp %>%
  filter(impute_obs == 1)

# impute, using defaults
wood_imp = impute_missing_values(wood_imp,
                                 col_nm_vec = c("length_m",
                                                "diameter_m"),
                                 method = "randomForestSRC",
                                 my_seed = 5,
                                 ntree = 1000,
                                 nk = 4)
# after impute
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

# In this case, for those site with a "sufficient" sample size, there does seem to be a slightly pos+ relationship
# btw length_m and diameter_m, which totally makes sense. In some cases, especially w/ insufficient sample size, not
# always the case. Probably makes most sense to just use length_m and diameter_m for imputation (i.e., use all data,
# no need to also use site). I mean, probably always a somewhat consistent relationship btw lenght_m and diameter_m
# for large woody debris, even despite the species of "wood" and site. Interesting conversation on plant evolution
# there! Let's impute length_m and diameter_m themselves, using default values. There doesn't seem to be any other
# vars in the rollup df that makes sense to use.

###########################################
# Wood: wet, ballasted, and channel_forming
wood_wbc = otg$wood %>%
  select(path_nm,
         parent_global_id,
         wet,
         channel_forming,
         ballasted) %>%
  mutate(path_nm = str_extract(path_nm, "[^_]+")) %>%
  mutate(impute_obs = ifelse(is.na(wet) | is.na(ballasted) | is.na(channel_forming),
                             1,
                             0) %>%
           as.factor())

wood_wbc %>%
  filter(impute_obs == 1)

miss_wbc = wood_wbc %>%
  filter(impute_obs == 1) %>%
  pull(parent_global_id)

wood_wbc %>%
  filter(parent_global_id %in% miss_wbc) %>%
  group_by(parent_global_id) %>%
  summarise(lwd_n_pieces = length(parent_global_id),
            lwd_p_wet = sum(wet == "Yes", na.rm = T) /
              sum(wet == "Yes" | wet == "No", na.rm = T),
            lwd_p_chn_frm = sum(channel_forming == "Yes", na.rm = T) /
              sum(channel_forming == "Yes" | channel_forming == "No", na.rm = T),
            lwd_p_ballast = sum(ballasted == "Yes", na.rm = T) /
              sum(ballasted == "Yes" | ballasted == "No", na.rm = T))

# In this case, perhaps this is the best we can and should do. We still need to roll up channel units to habitat
# reaches. Within channel units if we have the number of lwd pieces (lwd_n_pieces) and the proportions of those that
# are wet, channel_forming, and ballasted within each channel unit, that gives us flexibility calculating habitat
# reach metrics down the road...

#######################################
# Jam: estimated_number_of_pieces, length_m, width_m, height_m
jam_imp = otg$jam %>%
  select(path_nm,
         estimated_number_of_pieces,
         length_m,
         width_m,
         height_m) %>%
  mutate(path_nm = str_extract(path_nm, "[^_]+")) %>%
  mutate(impute_obs = ifelse(is.na(estimated_number_of_pieces) |
                               is.na(length_m) |
                               is.na(width_m) |
                               is.na(height_m),
                             1, 0) %>%
           as.factor())

# before imputation
jam_imp %>%
  filter(impute_obs == 1)

jam_imp = impute_missing_values(jam_imp,
                                col_nm_vec = c("estimated_number_of_pieces",
                                               "length_m",
                                               "width_m",
                                               "height_m"),
                                method = "randomForestSRC",
                                my_seed = 5,
                                ntree = 1000,
                                nk = 4)

# after imputation
jam_imp %>%
  filter(impute_obs == 1)

# This imputation seems very reasonable. But how best do we evaluate its performance without "simulating" more NAs.
# Maybe not worthwhile at this point as we only had 2 missing measurements? Imputing w/ defaults for now.

#######################################
# Undercut: length_m, width_25_percent_m, width_50_percent_m, width_75_percent_m
und_imp = otg$undercut %>%
  select(path_nm,
         parent_global_id,
         undercut_number,
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

# no missing values here. I added a message to the rollup functions to notify when no values are missing in impute_cols

# at this point, I feel good proceeding w/ the "primary" imputation using the default settings in the
# rollup functions and impute_missing_values, especially bc of so little missing data. Proceed...

rm(wood_imp,
   wood_wbc,
   jam_imp,
   und_imp)

#-------------------------
# roll up OTG data to CU scale, add primary data imputation
#-------------------------
cu_rllup_imp = otg_to_cu(survey_df = otg$survey,
                          cu_df = otg$cu,
                          wood_df = otg$wood,
                          jam_df = otg$jam,
                          undercut_df = otg$undercut,
                          discharge_df = otg$discharge,
                          discharge_meas_df = otg$discharge_measurements,
                          fix_nas = TRUE)

#-------------------------
# SECONDARY IMPUTE: Explore imputation of variables after running otg_to_cu()
#-------------------------
# It seems to me we need to consider imputing the following variable following the "primary" impute performed in
# otg_to_cu()
# 1. maximum_depth_m & thalweg_exit_depth_m
#      - except for OCA, in which case thalweg_exit_depth = NA is okay
# 2. ocular estimates: sand_fines_2mm:boulder_256mm
#      - these are mostly riffles. Protocol moving forward, I believe, is to collect ocular estimates in all
#      channel units, and so this will be largely resolved. But in 2019/2020, do we need to impute and/or
#      estimate ocular estimates, perhaps using information from pebble counts (where available)
# 3. fish cover: overhanging_cover:total_no_cover
# 4. side channel widths: width_1:width_5
#      - for small side channels only, right?

# maximum_depth_m, thalweg_exit_depth_m
cu_dpth_imp = cu_rllup_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_type,
         maximum_depth_m,
         thalweg_exit_depth_m) %>%
  # assign each CU as mainstem or side_channel
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(impute_obs = case_when(
    is.na(maximum_depth_m) ~ "1",
    is.na(thalweg_exit_depth_m) & channel_unit_type != "OCA" ~ "1",
    TRUE ~ "0") %>%
      as.factor()
    )

cu_dpth_imp %>%
  filter(impute_obs == 1)
# we need to impute maximum_depth_m & thalweg_exit_depth_m in these observations, perhaps using
# the variables in dpth_imp

# sand_fines_2mm:boulder_256mm
ocular_imp = cu_rllup_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_type,
         sand_fines_2mm:boulder_256mm,
         contains("pebble")) %>%
  # assign each CU as mainstem or side_channel
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(impute_obs = case_when(
    is.na(sand_fines_2mm) ~ "1",
    is.na(gravel_2_64mm) ~ "1",
    is.na(cobble_64_256mm) ~ "1",
    is.na(boulder_256mm) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
    )

# those w/ missing ocular estimates are nearly all riffles
ocular_imp %>%
  filter(impute_obs == 1) %>%
  tabyl(channel_unit_type) # nearly all riffles

# moving forward, I believe the protocol is to get ocular estimates at ALL channel units, but for 2019/2020
# Richie wanted us to attempt to impute these missing ocular estimates for riffles
# Fortunately, most of these riffles with missing values have pebble counts. Do we dare estimate ocular estimates
# using the pebble counts?
riffle_imp = ocular_imp %>%
  filter(channel_unit_type == "Riffle" & impute_obs == 1)

# overhanging_cover:total_no_cover
cover_imp = cu_rllup_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_type,
         overhanging_cover:total_no_cover) %>%
  # assign each CU as mainstem or side_channel
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  mutate(impute_obs = case_when(
    is.na(overhanging_cover) ~ "1",
    is.na(aquatic_vegetation) ~ "1",
    is.na(woody_debris_cover) ~ "1",
    is.na(artificial_cover) ~ "1",
    is.na(total_no_cover) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
  )

# before imputation
cover_imp %>%
  filter(impute_obs == 1) %>%
  View()

# how best do we impute and/or estimate these? Is drone imagery not available?

# width_1:width_5: If channel_segment_number != 01
sc_width_imp = cu_rllup_imp %>%
  select(path_nm,
         channel_segment_number,
         channel_unit_type,
         maximum_depth_m,
         thalweg_exit_depth_m,
         contains("width")) %>%
  # assign each CU as mainstem or side_channel
  mutate(segment_type = ifelse(channel_segment_number == "01",
                               "mainstem",
                               "side_channel")) %>%
  select(-channel_segment_number) %>%
  filter(segment_type == "side_channel",
         channel_unit_type == "SSC") %>%
  mutate(impute_obs = case_when(
    is.na(width_1) ~ "1",
    is.na(width_2) ~ "1",
    is.na(width_3) ~ "1",
    is.na(width_4) ~ "1",
    is.na(width_5) ~ "1",
    TRUE ~ "0") %>%
      as.factor()
  )

# only 13 cases
sc_width_imp %>%
  filter(impute_obs == 1)

# note, there's also some non-SSCs with widths, what's up with that?

#-------------------------
# write rolled up CU data, with "primary" and "secondary" imputations complete
#-------------------------


####################
# END 2/10/2021
####################
#-------------------------
# QC the rollup
#-------------------------
# qc_roll = qc_rollup(otg$survey,
#                     otg$cu,
#                     otg$jam,
#                     otg$undercut,
#                     otg$wood,
#                     otg$discharge,
#                     otg$discharge_measurements)
#
# qc_roll$error_df
#
# # what do the individual data look like for these dangling channel units?
# qc_roll$miss_rollup %>%
#   filter(source == "Wood") %>%
#   left_join(otg$wood)
#
# qc_roll$miss_rollup %>%
#   filter(source == "Jam") %>%
#   left_join(otg$jam)
#
# qc_roll$miss_rollup %>%
#   filter(source == "Undercut") %>%
#   left_join(otg$undercut)
#
# # where are the dangling discharge measurements?
# qc_roll$miss_discharge
