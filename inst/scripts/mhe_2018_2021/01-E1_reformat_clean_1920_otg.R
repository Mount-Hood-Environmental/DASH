# Authors: Mike Ackerman
#
# Purpose: Reformat the 2019 & 2020 OTG data to a format
#          that matches the 2018 & 2021 data i.e., to match
#          the newest DASH data collection forms
#
# Created: March 2, 2022
# Last Modified: March 24, 2022
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(magrittr)
library(janitor)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load QC'd OTG data from 2019 & 2020
#-------------------------
path_2019 = paste0(nas_prefix,
                   "main/data/habitat/DASH/OTG/2019")
path_2020 = paste0(nas_prefix,
                   "main/data/habitat/DASH/OTG/2020")

# list of otg_qcd.rda files in path_2019 and path_2020
otg_19_paths = list.files(path = path_2019,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2019, ., sep = "/")

otg_20_paths = list.files(path = path_2020,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2020, ., sep = "/")

otg_1920_paths = c(otg_19_paths, otg_20_paths)

# load each of otg_qcd.rda objects
otg_list = otg_1920_paths %>%
  map(.f = function(x) {
    load(x) %>%
      get()
  })

# combine elements of otg_list into otg
for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg = otg_list[[1]]
  } else {
    otg = suppressMessages(purrr::map2(otg,
                                       otg_list[[i]],
                                       dplyr::full_join))
  }
}

#-------------------------
# reformat to match newest data collection form
#-------------------------
# survey
otg$survey %<>%
  mutate(`Stream Name` = c("Big Springs Creek",
                           "Big Timber Creek",
                           "Big Timber Creek",
                           "Big Timber Creek",
                           "Bohannon Creek",
                           "Bohannon Creek",
                           "Canyon Creek",
                           "EF Bohannon Creek",
                           "Hawley Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Hayden Creek",
                           "Kenney Creek",
                           "Kenney Creek",
                           "Little Springs Creek",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "Lemhi River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "NF Salmon River",
                           "Big Springs Creek",
                           "Lemhi River",
                           "Grouse Creek",
                           "Lake Creek",
                           "Summit Creek"),
         HiddenStart = NA,
         HiddenEnd = NA,
         `Water Temp (C)` = NA,
         `Survey Date` = as.character(`Survey Date`),
         `CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`)) %>%
  rename(`Survey Start Date Time` = `Survey Date`) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Stream Name`,
         `Site Name`,
         `Survey Start Date Time`,
         HiddenStart,
         HiddenEnd,
         `Survey Crew`,
         `Water Temp (C)`,
         `Conductivity (ms)`,
         CreationDate,
         Creator,
         EditDate,
         Editor,
         x,
         y)

# cu
otg$cu %<>%
  mutate(TOS = FALSE,
         BOS = FALSE,
         `Cover Sum (%)` = rowSums(select(.,
                                          `Overhanging Cover`,
                                          `Aquatic Vegetation`,
                                          `Woody Debris Cover`,
                                          `Artificial Cover`,
                                          `Total No Cover`)),
         `Ocular Sum (%)` = rowSums(select(.,
                                           `Sand/Fines 2mm`,
                                           `Gravel 2-64mm`,
                                           `Cobble 64-256mm`,
                                           `Boulder 256mm`)),
         `CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`)) %>%
  rename(`Overhanging (%)` = `Overhanging Cover`,
         `Aquatic Vegetation (%)` = `Aquatic Vegetation`,
         `Woody Debris (%)` = `Woody Debris Cover`,
         `Artificial (%)` = `Artificial Cover`,
         `Total No Cover (%)` = `Total No Cover`,
         `Sand/Fines <2mm (%)`= `Sand/Fines 2mm`,
         `Gravel 2-64mm (%)` = `Gravel 2-64mm`,
         `Cobble 64-256mm (%)` = `Cobble 64-256mm`,
         `Boulder >256mm (%)` = `Boulder 256mm`,
         `Width 1 (m)` = `Width 1`,
         `Width 2 (m)` = `Width 2`,
         `Width 3 (m)` = `Width 3`,
         `Width 4 (m)` = `Width 4`,
         `Width 5 (m)` = `Width 5`) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Channel Segment Number`,
         `Channel Unit Number`,
         `Channel Unit Type`,
         `Maximum Depth (m)`,
         `Thalweg Exit Depth (m)`,
         `Channel Unit Notes`,
         TOS,
         BOS,
         `Overhanging (%)`,
         `Aquatic Vegetation (%)`,
         `Woody Debris (%)`,
         `Artificial (%)`,
         `Total No Cover (%)`,
         `Cover Sum (%)`,
         `Sand/Fines <2mm (%)`,
         `Gravel 2-64mm (%)`,
         `Cobble 64-256mm (%)`,
         `Boulder >256mm (%)`,
         `Ocular Sum (%)`,
         `Pebble 1 (mm)`:`Pebble 11 (mm)`,
         `Width 1 (m)`:`Width 5 (m)`,
         ParentGlobalID,
         CreationDate,
         Creator,
         EditDate,
         Editor)

# wood
otg$wood %<>%
  select(-`Large Wood Number`) %>%
  mutate(`CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`))

# jam
otg$jam %<>%
  mutate(`CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`))

# undercut
otg$undercut %<>%
  select(-`Undercut Number`) %>%
  mutate(`CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`))

# discharge
otg$discharge = otg$discharge_measurements %>%
  group_by(ParentGlobalID) %>%
  mutate(`Tape Distance (m)` = cumsum(`Station Width`)) %>%
  rename(`Station Depth (m)` = `Station Depth`,
         `Station Velocity (m/s)` = `Station Velocity`) %>%
  mutate(path_nm = str_replace(path_nm, "DischargeMeasurements_6", "Discharge_5"),
         `CreationDate` = as.character(`CreationDate`),
         `EditDate` = as.character(`EditDate`)) %>%
  select(path_nm,
         ObjectID,
         GlobalID,
         `Tape Distance (m)`,
         `Station Depth (m)`,
         `Station Velocity (m/s)`,
         ParentGlobalID,
         CreationDate,
         Creator,
         EditDate,
         Editor)

# remove discharge_measurements
otg$discharge_measurements = NULL

#-------------------------
# some additional cleaning of 2019/2020 OTG data to match new format
#-------------------------
# fill in some coordinates for surveys
otg$survey %<>%
  mutate(x = case_when(
    `Site Name` == "Big Springs" ~          -113.414252,
    `Site Name` == "Big Timber 1" ~         -113.373110,
    `Site Name` == "Big Timber 2" ~         -113.376794,
    `Site Name` == "Big Timber 3" ~         -113.395673,
    `Site Name` == "Canyon" ~               -113.359685,
    `Site Name` == "EF Bohannon" ~          -113.701099,
    `Site Name` == "Hawley" ~               -113.254670,
    `Site Name` == "Hayden 2" ~             -113.674917,
    `Site Name` == "Hayden 3" ~             -113.703991,
    `Site Name` == "Kenney 2" ~             -113.622160,
    `Site Name` == "Upper Lemhi 2" ~        -113.372243,
    `Site Name` == "Upper Lemhi 3" ~        -113.490779,
    `Site Name` == "Barton" ~               -113.993363,
    `Site Name` == "Upper Lemhi 3 Snyder" ~ -113.551993,
    TRUE ~ x
  )) %>%
  mutate(y = case_when(
    `Site Name` == "Big Springs" ~          44.716975,
    `Site Name` == "Big Timber 1" ~         44.695381,
    `Site Name` == "Big Timber 2" ~         44.662167,
    `Site Name` == "Big Timber 3" ~         44.598865,
    `Site Name` == "Canyon" ~               44.691027,
    `Site Name` == "EF Bohannon" ~          44.155091,
    `Site Name` == "Hawley" ~               44.662400,
    `Site Name` == "Hayden 2" ~             44.827973,
    `Site Name` == "Hayden 3" ~             44.787691,
    `Site Name` == "Kenney 2" ~             45.041051,
    `Site Name` == "Upper Lemhi 2" ~        44.699210,
    `Site Name` == "Upper Lemhi 3" ~        44.759344,
    `Site Name` == "Barton" ~               45.407768,
    `Site Name` == "Upper Lemhi 3 Snyder" ~ 44.785816,
    TRUE ~ y
  ))

# define TOS & BOS for 2019/2020 data
otg$cu %<>%
  mutate(TOS = case_when(
    ParentGlobalID == "96ff5d57-608b-435b-9df8-2fdd2e39909e" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "00306343-48cc-4c89-babe-904376f900b0" & `Channel Unit Number` == 76  ~ TRUE,
    ParentGlobalID == "0d1325cb-6d3c-417b-bdd9-dad987b0c04e" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "12603b8d-3bb1-4d69-9cd4-c7f5c924c347" & `Channel Unit Number` == 14  ~ TRUE,
    ParentGlobalID == "158b6a34-5643-4087-815f-c174214c2bfd" & `Channel Unit Number` == 99  ~ TRUE,
    ParentGlobalID == "28704a9c-a70a-4bbd-8fd0-09ba176ce1a5" & `Channel Unit Number` == 237 ~ TRUE,
    ParentGlobalID == "2eaecee5-8815-48b1-bfef-716cd8669796" & `Channel Unit Number` == 44  ~ TRUE,
    ParentGlobalID == "3095ed11-367b-49b9-a80b-32747e269e7d" & `Channel Unit Number` == 180 ~ TRUE,
    ParentGlobalID == "45145207-25b4-4c90-9cab-b335ebf36502" & `Channel Unit Number` == 126 ~ TRUE,
    ParentGlobalID == "4b6f3cb1-9635-448d-9551-8fc91a2cf699" & `Channel Unit Number` == 83  ~ TRUE,
    ParentGlobalID == "6465ebf7-a8a4-41e8-907f-66b7a769dbdd" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "69598648-7d97-421b-a854-96811e5a7578" & `Channel Unit Number` == 94  ~ TRUE,
    ParentGlobalID == "69a3a993-082a-4750-a3a9-9d798e0f3af6" & `Channel Unit Number` == 55  ~ TRUE,
    ParentGlobalID == "69bf44d5-742a-421c-845a-a9521937ebe7" & `Channel Unit Number` == 238 ~ TRUE,
    ParentGlobalID == "6ad772a2-4c8c-4eb4-887f-f98297e753e0" & `Channel Unit Number` == 512 ~ TRUE, # 2 TOS for Hawley (2 separate tribs)
    ParentGlobalID == "6ad772a2-4c8c-4eb4-887f-f98297e753e0" & `Channel Unit Number` == 452 ~ TRUE,
    ParentGlobalID == "722d1105-0e3a-44fe-b320-f9ef1a8dc6b3" & `Channel Unit Number` == 23  ~ TRUE,
    ParentGlobalID == "76728397-b587-4dad-9827-9973671b9deb" & `Channel Unit Number` == 22  ~ TRUE,
    ParentGlobalID == "771db2c8-4c8e-4df0-9839-b693888257a3" & `Channel Unit Number` == 94  ~ TRUE,
    ParentGlobalID == "79f1c5ac-5f9c-423b-8447-88b6e968f24a" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "7a86a0f0-ffde-408f-b041-bdb020fdb1e0" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "8283f953-20ae-4fdd-9e33-feef42407684" & `Channel Unit Number` == 314 ~ TRUE,
    ParentGlobalID == "8379fef7-eb15-4821-8b92-313e8e105df2" & `Channel Unit Number` == 72  ~ TRUE,
    ParentGlobalID == "8cf34b35-2943-4e98-939c-c588e966b707" & `Channel Unit Number` == 61  ~ TRUE,
    ParentGlobalID == "97e3f7c2-dd6c-4943-a114-3e3f6014fde8" & `Channel Unit Number` == 87  ~ TRUE,
    ParentGlobalID == "a8ff9117-d94c-4b95-a488-66e809b8fae5" & `Channel Unit Number` == 113 ~ TRUE,
    ParentGlobalID == "b86d3304-ca46-4ee0-ad39-2a90f764b272" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "c03aac81-2be9-4365-a5c4-94ce7abd5b3b" & `Channel Unit Number` == 297 ~ TRUE,
    ParentGlobalID == "c112afef-7423-445e-8dff-4e501a256092" & `Channel Unit Number` == 259 ~ TRUE,
    ParentGlobalID == "d5ab2e89-ccbb-420f-96a1-50981172ac75" & `Channel Unit Number` == 91  ~ TRUE,
    ParentGlobalID == "d68a7393-ce19-488d-a2e7-597c805d5afc" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "dd6cd709-e497-4996-8a65-b4e5d0fb1909" & `Channel Unit Number` == 31  ~ TRUE,
    ParentGlobalID == "e37464e5-295c-4667-b288-a544879dea4e" & `Channel Unit Number` == 138 ~ TRUE,
    ParentGlobalID == "eb816797-bd77-44f9-8f88-037f3318909d" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "ec73c35f-77ed-490e-a086-7241a19f55d2" & `Channel Unit Number` == 14  ~ TRUE,
    ParentGlobalID == "f53043c5-c346-42e9-9d52-3d7de5440459" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "f6b46437-3709-4914-ba10-6819d245f178" & `Channel Unit Number` == 1   ~ TRUE,
    TRUE ~ TOS
  )) %>%
  mutate(BOS = case_when(
    ParentGlobalID == "96ff5d57-608b-435b-9df8-2fdd2e39909e" & `Channel Unit Number` == 75  ~ TRUE,
    ParentGlobalID == "00306343-48cc-4c89-babe-904376f900b0" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "0d1325cb-6d3c-417b-bdd9-dad987b0c04e" & `Channel Unit Number` == 106 ~ TRUE,
    ParentGlobalID == "12603b8d-3bb1-4d69-9cd4-c7f5c924c347" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "158b6a34-5643-4087-815f-c174214c2bfd" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "28704a9c-a70a-4bbd-8fd0-09ba176ce1a5" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "2eaecee5-8815-48b1-bfef-716cd8669796" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "3095ed11-367b-49b9-a80b-32747e269e7d" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "45145207-25b4-4c90-9cab-b335ebf36502" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "4b6f3cb1-9635-448d-9551-8fc91a2cf699" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "6465ebf7-a8a4-41e8-907f-66b7a769dbdd" & `Channel Unit Number` == 69  ~ TRUE,
    ParentGlobalID == "69598648-7d97-421b-a854-96811e5a7578" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "69a3a993-082a-4750-a3a9-9d798e0f3af6" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "69bf44d5-742a-421c-845a-a9521937ebe7" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "6ad772a2-4c8c-4eb4-887f-f98297e753e0" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "722d1105-0e3a-44fe-b320-f9ef1a8dc6b3" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "76728397-b587-4dad-9827-9973671b9deb" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "771db2c8-4c8e-4df0-9839-b693888257a3" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "79f1c5ac-5f9c-423b-8447-88b6e968f24a" & `Channel Unit Number` == 116 ~ TRUE,
    ParentGlobalID == "7a86a0f0-ffde-408f-b041-bdb020fdb1e0" & `Channel Unit Number` == 137 ~ TRUE,
    ParentGlobalID == "8283f953-20ae-4fdd-9e33-feef42407684" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "8379fef7-eb15-4821-8b92-313e8e105df2" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "8cf34b35-2943-4e98-939c-c588e966b707" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "97e3f7c2-dd6c-4943-a114-3e3f6014fde8" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "a8ff9117-d94c-4b95-a488-66e809b8fae5" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "b86d3304-ca46-4ee0-ad39-2a90f764b272" & `Channel Unit Number` == 150 ~ TRUE,
    ParentGlobalID == "c03aac81-2be9-4365-a5c4-94ce7abd5b3b" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "c112afef-7423-445e-8dff-4e501a256092" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "d5ab2e89-ccbb-420f-96a1-50981172ac75" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "d68a7393-ce19-488d-a2e7-597c805d5afc" & `Channel Unit Number` == 65  ~ TRUE,
    ParentGlobalID == "dd6cd709-e497-4996-8a65-b4e5d0fb1909" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "e37464e5-295c-4667-b288-a544879dea4e" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "eb816797-bd77-44f9-8f88-037f3318909d" & `Channel Unit Number` == 184 ~ TRUE,
    ParentGlobalID == "ec73c35f-77ed-490e-a086-7241a19f55d2" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "f53043c5-c346-42e9-9d52-3d7de5440459" & `Channel Unit Number` == 136 ~ TRUE,
    ParentGlobalID == "f6b46437-3709-4914-ba10-6819d245f178" & `Channel Unit Number` == 116 ~ TRUE,
    TRUE ~ BOS
  )) %>%
  # standardize pebble sizes; replace all 1024 as 874 which is new as of new data collection form
  mutate(across(starts_with("Pebble"),
                ~if_else(. == 1024, 874, as.numeric(.)))) %>%
  # remove channel unit 56 from Lower Lemhi 3
  filter(GlobalID != "e03205cf-9fac-4a50-afce-c94baf3de5e9")

#-------------------------
# some additional cleaning of 2019/2020 OTG data to match new format
#-------------------------
# calculate ocular estimates based on pebble counts for riffles
ocular_tmp = otg$cu %>%
  filter(`Channel Unit Type` == "Riffle" & is.na(`Sand/Fines <2mm (%)`)) %>%
  rowwise() %>%
  mutate(`Sand/Fines <2mm (%)` = sum(c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) > 0 & c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) <= 2),
         `Gravel 2-64mm (%)` = sum(c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) > 2 & c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) <= 64),
         `Cobble 64-256mm (%)` = sum(c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) > 64 & c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) <= 256),
         `Boulder >256mm (%)` = sum(c_across(`Pebble 1 (mm)`:`Pebble 11 (mm)`) > 256)) %>%
  adorn_percentages(col = c(`Sand/Fines <2mm (%)`:`Boulder >256mm (%)`)) %>%
  mutate(`Ocular Sum (%)` = sum(c_across(`Sand/Fines <2mm (%)`:`Boulder >256mm (%)`))) %>%
  mutate_at(vars(`Sand/Fines <2mm (%)`:`Ocular Sum (%)`),
            .funs = funs(. * 100))

# replace riffles with missing ocular estimates with ocular_tmp above
otg$cu = anti_join(otg$cu,
                   ocular_tmp,
                   by = c("path_nm",
                          "GlobalID",
                          "Channel Unit Number")) %>%
  bind_rows(ocular_tmp)

# fill in some remaining missing values; here, I'm simply filling in missing values using the mean from each site and channel_unit_type
# not ideal, but perhaps I'll stew on some better methods.
otg$cu %<>%
  group_by(path_nm, `Channel Unit Type`) %>%
  # maximum and thalweg exit depths
  mutate_at(vars(`Maximum Depth (m)`:`Thalweg Exit Depth (m)`),
            ~ replace_na(., mean(., na.rm = TRUE))) %>%
  # fish cover
  mutate_at(vars(`Overhanging (%)`:`Cover Sum (%)`),
            ~ replace_na(., mean(., na.rm = TRUE))) %>%
  # ocular estimates
  mutate_at(vars(`Sand/Fines <2mm (%)`:`Ocular Sum (%)`),
            ~ replace_na(., mean(., na.rm = TRUE))) %>%
  # change some widths for all non-SSCs to NA
  ungroup() %>%
  mutate_at(
    vars(`Width 1 (m)`:`Width 5 (m)`),
    funs(case_when(
      `Channel Unit Type` != "SSC" ~ NA_real_,
      TRUE ~ .
    ))
  )

# after all this, I still have a small # of missing values; I'll attempt to fill these in using drone orthos

# one record with missing fish cover estimates
otg$cu %<>%
  mutate(`Overhanging (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 30,
    TRUE ~ `Overhanging (%)`)) %>%
  mutate(`Aquatic Vegetation (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 0,
    TRUE ~ `Aquatic Vegetation (%)`)) %>%
  mutate(`Woody Debris (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 0,
    TRUE ~ `Woody Debris (%)`)) %>%
  mutate(`Artificial (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 0,
    TRUE ~ `Artificial (%)`)) %>%
  mutate(`Total No Cover (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 70,
    TRUE ~ `Total No Cover (%)`)) %>%
  mutate(`Cover Sum (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 100,
    TRUE ~ `Cover Sum (%)`))

# one record with missing depths
otg$cu %<>%
  mutate(`Maximum Depth (m)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 0.2,
    TRUE ~ `Maximum Depth (m)`)) %>%
  mutate(`Thalweg Exit Depth (m)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 0.2,
    TRUE ~ `Thalweg Exit Depth (m)`))

# 16 SSCs with missing widths
otg$cu %<>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "BigSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 274 ~ 0.83,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "BigSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 274 ~ 1.16,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "BigSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 274 ~ 1.58,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "BigSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 274 ~ 1.46,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "BigSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 274 ~ 2.76,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Bohannon1_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 72 ~ 0.60,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Bohannon1_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 72 ~ 0.65,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Bohannon1_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 72 ~ 0.63,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Bohannon1_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 72 ~ 0.63,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Bohannon1_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 72 ~ 0.79,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 121 ~ 0.60,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 121 ~ 0.90,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 121 ~ 1.42,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 121 ~ 1.31,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 121 ~ 1.49,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 126 ~ 2.34,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 146 ~ 0.79,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 146 ~ 0.93,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 146 ~ 0.96,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 146 ~ 1.02,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 146 ~ 1.15,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 166 ~ 1.40,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 166 ~ 0.82,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 166 ~ 1.36,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 166 ~ 2,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Hawley_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 166 ~ 1.92,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Kenney2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 168 ~ 1.00,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Kenney2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 168 ~ 0.91,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Kenney2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 168 ~ 0.88,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Kenney2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 168 ~ 0.88,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Kenney2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 168 ~ 0.74,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 170 ~ 0.85,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 170 ~ 1.08,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 170 ~ 0.87,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 170 ~ 0.93,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 170 ~ 0.71,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 303 ~ 3.21,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 303 ~ 0.59,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 303 ~ 6.77,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 303 ~ 3.62,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 303 ~ 2.34,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 310 ~ 2.55,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 310 ~ 0.82,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 310 ~ 2.89,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 310 ~ 2.27,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 310 ~ 3.36,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "UpperLemhi2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 54 ~ 2.76,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "UpperLemhi2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 54 ~ 2.78,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "UpperLemhi2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 54 ~ 2.34,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "UpperLemhi2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 54 ~ 2.88,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "UpperLemhi2_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 54 ~ 2.95,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 14 ~ 0.9,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 14 ~ 0.79,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 14 ~ 1.17,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 14 ~ 0.86,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 14 ~ 0.94,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 15 ~ 1.03,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 15 ~ 0.84,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 15 ~ 1.06,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 15 ~ 1.04,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 15 ~ 1.11,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 53 ~ 1.32,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 53 ~ 1.90,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 53 ~ 1.06,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 53 ~ 1.97,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "UpperLemhi3_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 53 ~ 0.8,
    TRUE ~ `Width 5 (m)`)) %>%
  #----
  mutate(`Width 1 (m)` = case_when(
    path_nm == "Summit_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 18 ~ 0.80,
    TRUE ~ `Width 1 (m)`)) %>%
  mutate(`Width 2 (m)` = case_when(
    path_nm == "Summit_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 18 ~ 1.54,
    TRUE ~ `Width 2 (m)`)) %>%
  mutate(`Width 3 (m)` = case_when(
    path_nm == "Summit_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 18 ~ 0.64,
    TRUE ~ `Width 3 (m)`)) %>%
  mutate(`Width 4 (m)` = case_when(
    path_nm == "Summit_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 18 ~ 0.73,
    TRUE ~ `Width 4 (m)`)) %>%
  mutate(`Width 5 (m)` = case_when(
    path_nm == "Summit_Survey123_2020/CU_1.csv" & `Channel Unit Number` == 18 ~ 0.87,
    TRUE ~ `Width 5 (m)`)) %>%
  # also found this error while filling in missing SSC widths; channel unit was mis-classified as SSC
  mutate(`Channel Segment Number` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 57 ~ 1L,
    TRUE ~ `Channel Segment Number`)) %>%
  mutate(`Channel Unit Type` = case_when(
    path_nm == "LittleSprings_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 57 ~ "Run",
    TRUE ~ `Channel Unit Type`))

# 1 record with missing ocular estimates
otg$cu %<>%
  mutate(`Sand/Fines <2mm (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 30,
    TRUE ~ `Sand/Fines <2mm (%)`)) %>%
  mutate(`Gravel 2-64mm (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 40,
    TRUE ~ `Gravel 2-64mm (%)`)) %>%
  mutate(`Cobble 64-256mm (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 20,
    TRUE ~ `Cobble 64-256mm (%)`)) %>%
  mutate(`Boulder >256mm (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 10,
    TRUE ~ `Boulder >256mm (%)`)) %>%
  mutate(`Ocular Sum (%)` = case_when(
    path_nm == "Hayden4_Survey123_2019/CU_1.csv" & `Channel Unit Number` == 24 ~ 100,
    TRUE ~ `Ocular Sum (%)`))

# save results
saveRDS(otg,
        file = paste0(nas_prefix,
                      "main/data/habitat/DASH/OTG/prepped/otg_qcd_1920_new_format.rds"))

# END SCRIPT
