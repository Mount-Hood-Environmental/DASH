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
