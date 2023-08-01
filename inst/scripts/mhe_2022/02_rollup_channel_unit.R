# Authors: Mike Ackerman
#
# Purpose: rollup OTG data to channel unit scale
#
# Created: March 4, 2022
#   Last Modified: August 1, 2023 by Tulley Mackey for DASH data collected
#                 July, 2022 near Lemhi_Hayden confluence
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
# load 2022 OTG data
#-------------------------

otg_2022 = readRDS(file = paste0(otg_path,
                                 "prepped/otg_qcd_2022.rds"))

# remove the qc_results data frame from otg_qcd_2022
otg_2022$qc_results = NULL

#-------------------------
# roll up OTG data to CU scale, no data imputation
#-------------------------
otg_22_cu = otg_to_cu(survey_df = otg_2022$survey,
                   cu_df = otg_2022$cu,
                   wood_df = otg_2022$wood,
                   jam_df = otg_2022$jam,
                   undercut_df = otg_2022$undercut,
                   discharge_df = otg_2022$discharge,
                   fix_nas = FALSE) # no imputation

# save non-imputed, prepped data
write_rds(otg_22_cu,
          paste0(otg_path,
                 "prepped/dash_2022_cu_no_impute.rds"))

#-------------------------
# roll up OTG data to CU scale, add primary data imputation
#-------------------------
otg_22_cu_imp = otg_to_cu(survey_df = otg_2022$survey,
                       cu_df = otg_2022$cu,
                       wood_df = otg_2022$wood,
                       jam_df = otg_2022$jam,
                       undercut_df = otg_2022$undercut,
                       discharge_df = otg_2022$discharge,
                       fix_nas = TRUE) # impute missing values

# save imputed, prepped data
write_rds(otg_22_cu_imp,
          paste0(otg_path,
                 "prepped/dash_2022_cu_imputed.rds"))

# END SCRIPT
