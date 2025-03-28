# Authors: Bryce Oldemeyer, Bridger Bertram, Tulley Mackey
# Co-author: Mike Ackerman
#
# Purpose: rollup OTG data to channel unit scale
#
# Created: March 4, 2022
# Last Modified: Bridger Bertram

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(DASH)
library(here)

#-----------------------------
# set some arguments/parameters
#-----------------------------
#set nas prefix
#if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

# path to example OTG data
otg_path = here("data/example_data")

# path to project OTG data
# otg_path = here("data/project_data")

#-------------------------
# load OTG data
#-------------------------


#year = "2024" # example year, Put in your year here
#watershed = "example" # example watershed, put your watershed here

# This will read in the OTG data for your project in the 3_prepped_otg folder.
otg_data <- readRDS(file = paste0(otg_path,"/3_prepped_otg/otg_qcd.rds"))


# remove the qc_results data frame from otg results
otg_data$qc_results = NULL

#-------------------------
# roll up OTG data to CU scale, no data imputation
#-------------------------

otg_data_cu = otg_to_cu(survey_df = otg_data$survey,
                   cu_df = otg_data$cu,
                   wood_df = otg_data$wood,
                   jam_df = otg_data$jam,
                   undercut_df = otg_data$undercut,
                   discharge_df = otg_data$discharge,
                   fix_nas = FALSE) # no imputation

# save non-imputed, prepped data
write_rds(otg_data_cu,
          paste0(otg_path,"/4_otg_rolled_cu/cu_no_impute.rds"))

#-------------------------
# roll up OTG data to CU scale, add primary data imputation
#-------------------------

otg_data_cu_imp = otg_to_cu(survey_df = otg_data$survey,
                       cu_df = otg_data$cu,
                       wood_df = otg_data$wood,
                       jam_df = otg_data$jam,
                       undercut_df = otg_data$undercut,
                       discharge_df = otg_data$discharge,
                       fix_nas = TRUE) # impute missing values


# save imputed, prepped data
write_rds(otg_data_cu,
          paste0(otg_path,"/4_otg_rolled_cu/cu_imputed.rds"))

# END SCRIPT
