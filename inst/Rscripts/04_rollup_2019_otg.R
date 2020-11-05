# Authors: Mike Ackerman
#
# Purpose: A script to clean and prep QC'd 2019 on-the-ground (OTG) DASH data.
# This script follows others to import and QC the raw data.
# Created: October 6, 2020
#   Lat Modified: October 6, 2020
#
# Notes:
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
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

#-----------------------------
# load the otg_qcd list of dfs for lemhi and nf salmon
#-----------------------------
lemhi_otg = load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/prepped/qcd_DASH_2019_otg.rda")) %>%
  get()
nfsal_otg = load(paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/nf_salmon/prepped/qcd_DASH_2019_otg.rda")) %>%
  get()

# and merge them
otg = purrr::map2(lemhi_otg,
                  nfsal_otg,
                  dplyr::full_join)

# clean up
rm(lemhi_otg, nfsal_otg, otg_qcd)

# write joined data
# output_path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/prepped/")
# save(otg,
#      file = paste0(output_path, "DASH_2019_otg.rda"))
#
# # and csvs (for now)
# write_csv(otg$survey, paste0(output_path, "survey.csv"))
# write_csv(otg$cu, paste0(output_path, "cu.csv"))
# write_csv(otg$wood, paste0(output_path, "wood.csv"))
# write_csv(otg$jam, paste0(output_path, "jam.csv"))
# write_csv(otg$undercut, paste0(output_path, "undercut.csv"))
# write_csv(otg$discharge, paste0(output_path, "discharge.csv"))
# write_csv(otg$discharge_measurements, paste0(output_path, "discharge_measurements.csv"))

#-----------------------------
# clean cu data and join site info to it
#-----------------------------
# clean survey names
otg$survey = otg$survey %>%
  mutate(site_name = gsub("_.*", "", site_name)) %>%
  mutate(site_name = gsub(" ", "", site_name))

# CU
cu_cu = rollup_cu(cu_df = otg$cu,
                  survey_df = otg$survey)

#-----------------------------
# start rolling up data to cu scale
#-----------------------------
# WOOD
cu_wood = rollup_cu_wood(wood_df = otg$wood)

# JAM
cu_jam = rollup_cu_jam(jam_df = otg$jam)

# UNDERCUT
cu_undercut = rollup_cu_undercut(undercut_df = otg$undercut)

# DISCHARGE
cu_discharge = rollup_cu_discharge(discharge_df = otg$discharge,
                                   discharge_meas_df = otg$discharge_measurements) %>%
  # Add the below to the rollup_cu_discharge() function?
  left_join(otg$survey %>%
              select(global_id,
                     site_name),
            by = c("parent_global_id" = "global_id")) %>%
  mutate(cu_id = paste(site_name,
                       "01",
                       str_pad(discharge_location_bos_tos_cu_number, 3, pad = "0"),
                       sep = "_")) %>%
  select(site_name,
         cu_id,
         discharge_cms,
         discharge_location_bos_tos_cu_number,
         everything()) %>%
  left_join(cu_cu %>%
              select(parent_global_id,
                     cu_id,
                     channel_unit_number) %>%
              group_by(parent_global_id) %>%
              summarise(min_cu = min(channel_unit_number),
                        max_cu = max(channel_unit_number))) %>%
  select(cu_id,
         discharge_cms)

# Here, I need to resolve the BOS and TOS issue. How do I assign a CU number to the BOS and TOS discharge measurements?

#-----------------------------
# join everything together at cu scale
#-----------------------------
dash_otg_cu = list(cu_cu %>%
                     rename(site_id = global_id),
                   cu_wood,
                   cu_jam,
                   cu_undercut) %>%
  purrr::reduce(left_join,
                by = "parent_global_id") %>%
  left_join(cu_discharge)

#-----------------------------
# write results
#-----------------------------
save(dash_otg_cu,
     file = paste0(nas_prefix, "data/habitat/DASH/OTG/2019/prepped/DASH_2019_otg_cu.rda"))

write_csv(dash_otg_cu,
          paste0(nas_prefix, "data/habitat/DASH/OTG/2019/prepped/DASH_2019_otg_cu.csv"))
