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

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
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
otg_qcd = purrr::map2(lemhi_otg,
                      nfsal_otg,
                      dplyr::full_join)

# clean up
rm(lemhi_otg, nfsal_otg)

