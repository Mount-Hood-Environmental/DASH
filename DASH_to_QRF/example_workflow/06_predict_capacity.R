# Author: Bryce Oldemeyer, Bridger Bertram, Tulley Mackey
# Purpose: Estimate habitat capacity for DASH surveyed sites
# Created: 1/30/2024
# Last Modified: 5/06/2024
# Modified by: Bridger Bertram

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
# set data directory
#-------------------------

data_directory <- here("data/example_data")
#data_directory <- here("data/project_data")

#-------------------------
# Read in Prepped DASH Data
#-------------------------

qrf_hab_reach <- read.csv(paste0(data_directory,"/7_qrf_ready/qrf_hab_reach.csv"))
qrf_chnl_unit <- read.csv(paste0(data_directory,"/7_qrf_ready/qrf_chnl_unit.csv"))

# Function to impute missing data
source("R/impute_missing_data.R")

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
# Save Predictions
#-------------------------

write_csv(juvenile_summer_preds, paste0(data_directory,"/8_qrf_preds/juvenile_summer.csv"))
write_csv(redds_preds, paste0(data_directory,"/8_qrf_preds/redds.csv"))

### End Script


