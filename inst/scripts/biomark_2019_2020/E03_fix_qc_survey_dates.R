# Author: Kevin See
# Purpose: Fix survey dates on 2_qcd_csv files
# Created: 1/20/21
# Last Modified: 1/21/21
# Notes: This should only need to be run once, and only because somehow the dates in the 2_qcd_csv files were corrupted

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(beepr)
library(janitor)
library(DASH)
# devtools::load_all()

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-----------------------------
# set some arguments/parameters
#-----------------------------
# create a vector to directories containing the raw data, formatted for import
yr_wtsd = c("2019/lemhi",
            "2019/nf_salmon",
            "2020/lemhi",
            "2020/secesh")


#-----------------------------
# loop over year_watershed combinations
#-----------------------------
for (yw in yr_wtsd) {

  # set the directories
  path_format = paste0(nas_prefix,
                       "/data/habitat/DASH/OTG/",
                       yw,
                       "/1_formatted_csvs/")

  path_qcd = paste0(nas_prefix,
                    "/data/habitat/DASH/OTG/",
                    yw,
                    "/2_qcd_csvs/")
  # path_qcd = "~/Desktop/2_qcd_csvs/"

  # read in the data from the 2_qcd_csv folder
  otg_qcd = read_otg_csv_wrapper(path = path_qcd,
                                 otg_type_list = c("surveyPoint_0.csv"),
                                 otg_type_names = "survey")
                                 # otg_type_list = c("surveyPoint_0.csv",
                                 #                   "CU_1.csv",
                                 #                   "Wood_2.csv",
                                 #                   "Jam_3.csv",
                                 #                   "Undercut_4.csv",
                                 #                   "Discharge_5.csv",
                                 #                   "DischargeMeasurements_6.csv"),
                                 # otg_type_names = c("survey",
                                 #                    "cu",
                                 #                    "wood",
                                 #                    "jam",
                                 #                    "undercut",
                                 #                    "discharge",
                                 #                    "discharge_measurements"))

  # overwrite csvs with updated values
  otg_qcd %>%
    walk(.f = function(x) {
      # fix issue with writing date from survey back to csv (grab date from 1_formatted_csv folder)
      if("Survey Date" %in% names(x)) {
        x = x %>%
          mutate(`Survey Date` = map_chr(path_nm,
                                         .f = function(y) {
                                           suppressMessages(read_csv(paste0(path_format, y))) %>%
                                             pull(`Survey Date`)
                                         }))
      }

      if("CreationDate" %in% names(x)) {
        x = x %>%
          mutate(CreationDate = map_chr(path_nm,
                                         .f = function(y) {
                                           suppressMessages(read_csv(paste0(path_format, y))) %>%
                                             pull(CreationDate)
                                         }),
                 EditDate = map_chr(path_nm,
                                    .f = function(y) {
                                      suppressMessages(read_csv(paste0(path_format, y))) %>%
                                        pull(EditDate)
                                    }))
      }

      x %>%
        group_by(path_nm) %>%
        group_split() %>%
        map(.f = function(y) {
          y %>%
            select(-path_nm) %>%
            write_csv(paste0(path_qcd, unique(y$path_nm)))
          return(NULL)
        })
      return(NULL)
    })

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }
}
