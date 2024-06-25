# Author: Bryce Oldemeyer, Tulley Mackey
# Purpose: Estimate habitat capacity for DASH surveyed sites
# Created: 1/30/2024
# Last Modified: 5/06/2024
# Modified by: Bridger Bertram for site specific DASH - QRF pipeline
#
#
# clear environment

rm(list = ls())

# load needed libraries

library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)
library(readxl)
library(here)
library(patchwork)
library(ggforce)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------

if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }

# Specify year and site
year = "2024"
watershed = "example"

#-------------------------
# Read in Prepped DASH Data
#-------------------------

qrf_hab_reach <- read.csv(paste0(nas_prefix,"main/data/habitat/DASH/DASH_prepped_for_QRF/",year,"/",watershed,"/habitat_reach_scale"))
qrf_chnl_unit <- read.csv(paste0(nas_prefix,"main/data/habitat/DASH/DASH_prepped_for_QRF/",year,"/",watershed,"/channel_unit_scale"))

# Function to impute missing data
source("R/impute_missing_data.r")

#-------------------------
# Juvenile Summer Rearing
#-------------------------

# ~~~~~ Select Model ~~~~~ #

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[1]

load(paste0('S:/main/data/qrf/gitrepo_data/output/modelFit/', mod_choice, '_No_elev.rda'))

pred_quant <- 0.9

# ~~~~~ Make Predictions ~~~~~ #
juvenile_summer_preds = qrf_hab_reach %>%
  st_drop_geometry() %>%
  mutate(WetBraid = case_when(
    WetBraid > 2 ~ 2,
    WetBraid < 1 ~ 1,
    TRUE ~ WetBraid),
    PoolResidDpth = replace_na(PoolResidDpth, 0)) %>%
  mutate(chnk_per_m_juv_sum = predict(qrf_mods[['Chinook']],
                                      newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                      what = pred_quant),
         chnk_per_m_juv_sum = exp(chnk_per_m_juv_sum) - dens_offset) %>%
  mutate(sthd_per_m_juv_sum = predict(qrf_mods[['Steelhead']],
                                      newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                      what = pred_quant),
         sthd_per_m_juv_sum = exp(sthd_per_m_juv_sum) - dens_offset) %>%
  mutate(chnk_cap_juv_sum = chnk_per_m_juv_sum * Lgth_Wet,
         sthd_cap_juv_sum = sthd_per_m_juv_sum * Lgth_Wet) %>%
  mutate(chnk_per_m2_juv_sum = chnk_cap_juv_sum / area,
         sthd_per_m2_juv_sum = sthd_cap_juv_sum / area)

#-------------------------
# Adult Spawning (Redds)
#-------------------------

# ~~~~~ Select Model ~~~~~ #

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[2]

load(paste0('S:/main/data/qrf/gitrepo_data/output/modelFit/', mod_choice, '_No_elev.rda'))

pred_quant <- 0.9

# ~~~~~ Make Predictions ~~~~~ #

redds_preds = qrf_hab_reach %>%
  mutate(PoolResidDpth = replace_na(PoolResidDpth, 0)) %>%
  mutate(chnk_per_km_redds = predict(qrf_mods[['Chinook']],
                                     newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                     what = pred_quant),
         chnk_per_km_redds = (exp(chnk_per_km_redds) - dens_offset)*1000) %>%
  mutate(sthd_per_km_redds = predict(qrf_mods[['Steelhead']],
                                     newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                     what = pred_quant),
         sthd_per_km_redds = (exp(sthd_per_km_redds) - dens_offset)*1000) %>%
  mutate(chnk_cap_redds = chnk_per_km_redds * (Lgth_Wet/1000),
         chnk_per_km2_redds = chnk_cap_redds/(area/1000),
         sthd_cap_redds = sthd_per_km_redds * (Lgth_Wet/1000),
         sthd_per_km2_redds = sthd_cap_redds/(area/1000))

#-------------------------
# Juvenile Winter Rearing
#-------------------------

# ~~~~~ Select Model ~~~~~ #

mod_choice = c('juv_summer',
               'redds',
               'juv_winter')[3]

load(paste0('S:/main/data/qrf/gitrepo_data/output/modelFit/', mod_choice, '_No_elev.rda'))

pred_quant <- 0.9

# ~~~~~ Make Predictions ~~~~~ #

impute_covars = c("Sin","end_elev","region","slope")

juvenile_winter_preds = qrf_chnl_unit %>%
  st_drop_geometry() %>%
  impute_missing_data(covars = c("DpthResid","DpthThlwgExit", "FishCovLW", "FishCovSome","SubEstCandBldr",
                                "SubEstGrvl", "SubEstSandFines","Q","WetBraid"),
                      impute_vars = impute_covars,
                      method = 'missForest') %>%
  mutate(WetBraid = case_when(
    WetBraid > 2 ~ 2,
    WetBraid < 1 ~ 1,
    TRUE ~ WetBraid
  )) %>%
  mutate(chnk_per_m2_juv_win = predict(qrf_mods[['Chinook']],
                                       newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                       what = pred_quant),
         chnk_per_m2_juv_win = exp(chnk_per_m2_juv_win) - dens_offset) %>%
  mutate(sthd_per_m2_juv_win = predict(qrf_mods[['Steelhead']],
                                       newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                                       what = pred_quant),
         sthd_per_m2_juv_win = exp(sthd_per_m2_juv_win) - dens_offset) %>%
  mutate(chnk_cap_juv_win = chnk_per_m2_juv_win * area,
         sthd_cap_juv_win = sthd_per_m2_juv_win * area) %>%
  mutate(chnk_per_m_juv_win = chnk_cap_juv_win/Lgth_Wet,
         sthd_per_m_juv_win = sthd_cap_juv_win/Lgth_Wet)

#-------------------------
# Save Predictions
#-------------------------

write_csv(juvenile_summer_preds, paste0(nas_prefix,"main/data/habitat/DASH/capacity_estimates/",year,"/",watershed,"/juvenile_summer_rearing.csv"))
write_csv(juvenile_winter_preds, paste0(nas_prefix,"main/data/habitat/DASH/capacity_estimates/",year,"/",watershed,"/juvenile_winter_rearing.csv"))
write_csv(redds_preds, paste0(nas_prefix,"main/data/habitat/DASH/capacity_estimates/",year,"/",watershed,"/redds.csv"))

### End Script


