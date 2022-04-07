# Authors: Mike Ackerman
#
# Purpose: rollup OTG data to channel unit scale
#
# Created: March 4, 2022
#   Last Modified: March 23, 2022
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

#-----------------------------
# set some arguments/parameters
#-----------------------------
#set nas prefix
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

# path to OTG data on the NAS
otg_path = paste0(nas_prefix, "main/data/habitat/DASH/OTG/")

#-------------------------
# load all OTG data
#-------------------------
otg = readRDS(file = paste0(otg_path,
                            "prepped/otg_all_18to21.rds"))
# remove the qc_results data frame from otg
otg$qc_results = NULL

#-------------------------
# roll up OTG data to CU scale, no data imputation
#-------------------------
otg_cu = otg_to_cu(survey_df = otg$survey,
                   cu_df = otg$cu,
                   wood_df = otg$wood,
                   jam_df = otg$jam,
                   undercut_df = otg$undercut,
                   discharge_df = otg$discharge,
                   fix_nas = FALSE) # no imputation

# save non-imputed, prepped data
write_rds(otg_cu,
          paste0(otg_path,
                 "prepped/dash_18to21_cu_no_impute.rds"))

#-------------------------
# roll up OTG data to CU scale, add primary data imputation
#-------------------------
otg_cu_imp = otg_to_cu(survey_df = otg$survey,
                       cu_df = otg$cu,
                       wood_df = otg$wood,
                       jam_df = otg$jam,
                       undercut_df = otg$undercut,
                       discharge_df = otg$discharge,
                       fix_nas = TRUE) # impute missing values

# save imputed, prepped data
write_rds(otg_cu_imp,
          paste0(nas_prefix,
                 otg_path,
                 "prepped/dash_18to21_cu_imputed.rds"))

# END SCRIPT


#-------------------------
# PRIMARY IMPUTE: Explore imputation of variables in wood_df, jam_df, and undercut_df
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


# END SCRIPT
