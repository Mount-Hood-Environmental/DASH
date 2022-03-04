# Authors: Mike Ackerman
#
# Purpose: Join the 2019/2020 data (old format) to 2018/2021 data (new format)
# and then rollup OTG data to channel-unit scale
#
#
# Created: March 2, 2022
# Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(janitor)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type == "windows") { nas_prefix = "S:/" }
# need additional statements if someone has an alternative OS.type

#-------------------------
# load QC'd OTG data from 2018 & 2021
#-------------------------
path_2018 = paste0(nas_prefix,
                   "Public Data/data/habitat/DASH/OTG/2018")
path_2021 = paste0(nas_prefix,
                   "Public Data/data/habitat/DASH/OTG/2021")

# list of otg_qcd.rda files in path_2019 and path_2020
otg_18_paths = list.files(path = path_2018,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2018, ., sep = "/")

otg_21_paths = list.files(path = path_2021,
                          pattern = "^otg_qcd.rda$",
                          recursive = T) %>%
  paste(path_2021, ., sep = "/")

otg_1821_paths = c(otg_18_paths, otg_21_paths)

# load each of otg_qcd.rda objects
otg_list = otg_1821_paths %>%
  map(.f = function(x) {
    load(x) %>%
      get()
  })

# clean some stuff
rm(otg_18_paths, otg_21_paths,
   path_2018, path_2021,
   otg_1821_paths)

# add some blank jam tibbles for a couple cases where there are no data
otg_list[[2]]$jam = tibble("path_nm" = as.character(),
                           "ObjectID" = as.integer(),
                           "GlobalID" = as.character(),
                           "Length (m)" = as.double(),
                           "Width (m)" = as.double(),
                           "Height (m)" = as.double(),
                           "Estimated Number of Pieces" = as.integer(),
                           "ParentGlobalID" = as.character(),
                           "CreationDate" = as.character(),
                           "Creator" = as.character(),
                           "Edit Date" = as.character(),
                           "Editor" = as.character())
otg_list[[3]]$jam = tibble("path_nm" = as.character(),
                           "ObjectID" = as.integer(),
                           "GlobalID" = as.character(),
                           "Length (m)" = as.double(),
                           "Width (m)" = as.double(),
                           "Height (m)" = as.double(),
                           "Estimated Number of Pieces" = as.integer(),
                           "ParentGlobalID" = as.character(),
                           "CreationDate" = as.character(),
                           "Creator" = as.character(),
                           "Edit Date" = as.character(),
                           "Editor" = as.character())
otg_list[[2]] = otg_list[[2]][c("survey", "cu", "wood", "jam", "undercut", 'discharge', "qc_results")]
otg_list[[3]] = otg_list[[3]][c("survey", "cu", "wood", "jam", "undercut", 'discharge', "qc_results")]

# combine elements of otg_list into otg
for(i in 1:length(otg_list)) {
  if(i == 1) {
    otg_1821 = otg_list[[1]]
  } else {
    otg_1821 = suppressMessages(purrr::map2(otg_1821,
                                       otg_list[[i]],
                                       dplyr::full_join))
  }
}

#-------------------------
# load QC'd OTG data from 2019 & 2020
#-------------------------
otg_1920 = readRDS(file = paste0(nas_prefix,
                                 "Public Data/data/habitat/DASH/OTG/prepped/otg_qcd_1920_new_format.rds"))

#-------------------------
# bind 18/21 & 19/20 OTG datasets
#-------------------------
otg_all_list = list(otg_1821, otg_1920)
for(i in 1:length(otg_all_list)) {
  if(i == 1) {
    otg_all = otg_all_list[[1]]
  } else {
    otg_all = suppressMessages(purrr::map2(otg_all,
                                           otg_all_list[[i]],
                                           dplyr::full_join))
  }
}

#-------------------------
# let's do some exploring/cleaning of otg_all
#-------------------------

# fill in some coordinates in survey
otg_all$survey = otg_all$survey %>%
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

tmp = otg_all$cu %>%
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
    ParentGlobalID == "c03aac81-2be9-4365-a5c4-94ce7abd5b3b" & `Channel Unit Number` == 317 ~ TRUE,
    ParentGlobalID == "c112afef-7423-445e-8dff-4e501a256092" & `Channel Unit Number` == 259 ~ TRUE,
    ParentGlobalID == "d5ab2e89-ccbb-420f-96a1-50981172ac75" & `Channel Unit Number` == 91  ~ TRUE,
    ParentGlobalID == "d68a7393-ce19-488d-a2e7-597c805d5afc" & `Channel Unit Number` == 1   ~ TRUE,
    ParentGlobalID == "dd6cd709-e497-4996-8a65-b4e5d0fb1909" & `Channel Unit Number` == 31  ~ TRUE,
    ParentGlobalID == "e37464e5-295c-4667-b288-a544879dea4e" & `Channel Unit Number` == 138 ~ TRUE,
    ParentGlobalID == "eb816797-bd77-44f9-8f88-037f3318909d" & `Channel Unit Number` == 1   ~ TRUE,
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
    TRUE ~ BOS
  ))

# save results
saveRDS(otg_all,
        file = paste0(nas_prefix,
                      "Public Data/data/habitat/DASH/OTG/prepped/otg_all_18to21.rds"))

# END SCRIPT
