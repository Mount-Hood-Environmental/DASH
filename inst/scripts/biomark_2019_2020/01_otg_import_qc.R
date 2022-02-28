# Authors: Mike Ackerman & Kevin See
#
# Purpose: A script to import and QC all of the 2019 & 2020 on-the-ground (OTG)
# DASH data collected using Survey123.
#
# Created: July 15, 2020
#   Last Modified: January 25, 2021
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(tidyverse)
library(beepr)
library(janitor)
library(DASH)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
} else if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-----------------------------
# Do you need to copy initial formatted files to a QC'd folder?
#-----------------------------
# if copy_to_qcd is set to TRUE, the files contained in the /1_formatted_csvs/
# folder will be copied into a /2_qcd_csvs/ folder to have QC edits made.
# If /2_qcd_csvs/ folders already exist, this will overwrite what's in there.
# So please set to TRUE only the first time running this script!
copy_to_qcd = FALSE

#-----------------------------
# set some arguments/parameters
#-----------------------------
# create a vector to directories containing the raw data, formatted for import
# 2018 & 2021 data (updated data collection forms)
yr_wtsd = c("2018/lemhi",
            "2018/pahsimeroi",
            "2018/upper_salmon",
            "2021/big_lost",
            "2021/mf_salmon",
            "2021/nf_salmon")

# 2019 & 2020 data are in old data collection format
# yr_wtsd = c(yr_wtsd,
#             "2019/lemhi",
#             "2019/nf_salmon",
#             "2020/lemhi",
#             "2020/secesh")

#-----------------------------
# LOOP 1: import raw OTG data; loop over year_watershed combinations
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd
  path = paste0(nas_prefix,
                "/Public Data/data/habitat/DASH/OTG/",
                yw,
                "/1_formatted_csvs/")

  # loop over OTG data types using the wrapper function
  otg_raw = read_otg_csv_wrapper(path = path,
                                 otg_type_list = c("surveyPoint_0.csv",
                                                   "CU_1.csv",
                                                   "Wood_2.csv",
                                                   "Jam_3.csv",
                                                   "Undercut_4.csv",
                                                   "Discharge_5.csv"),
                                 otg_type_names = c("survey",
                                                    "cu",
                                                    "wood",
                                                    "jam",
                                                    "undercut",
                                                    "discharge"))

  # QC all data types for each yr_wtsd
  otg_raw$qc_results = qc_wrapper(survey_df = otg_raw$survey,
                                  cu_df = otg_raw$cu,
                                  wood_df = otg_raw$wood,
                                  jam_df = otg_raw$jam,
                                  undercut_df = otg_raw$undercut,
                                  discharge_df = otg_raw$discharge,
                                  #discharge_meas_df = otg_raw$discharge_measurements,
                                  channel_unit_roll_qc = TRUE)

  #-----------------------------
  # if you need to copy the files into a QC'd series of folders
  # this should only happen the first time you run this script
  #-----------------------------
  if(copy_to_qcd) {

    # where will QC'd files go
    path_qcd = paste0(nas_prefix,
                      "/Public Data/data/habitat/DASH/OTG/",
                      yw,
                      "/2_qcd_csvs/")

    # create /2_qcd_csvs/ folder
    if(!dir.exists(path_qcd)) {
      dir.create(path_qcd)
    }

    # create folders for each survey
    survy_fldrs = otg_raw %>%
      map_df(.id = 'source',
             .f = function(x) {
               x %>%
                 select(path_nm) %>%
                 distinct()
             }) %>%
      mutate(fldr_nm = str_split(path_nm, "/", simplify = T)[,1]) %>%
      pull(fldr_nm) %>%
      unique()
    for(i in 1:length(survy_fldrs)) {
      if(!dir.exists(paste0(path_qcd, survy_fldrs[i]))) {
        dir.create(paste0(path_qcd, survy_fldrs[i]))
      }
    }

    # copy csvs into appropriate QC folder
    otg_raw %>%
      map_df(.id = 'source',
             .f = function(x) {
               x %>%
                 select(path_nm) %>%
                 distinct()
             }) %>%
      pull(path_nm) %>%
      as.list() %>%
      walk(.f = function(x) {
        file.copy(from = paste0(path, x),
                  to = paste0(path_qcd, x))
      })
  } # end if copy_to_qcd = TRUE

  # save the otg_raw list of dfs, and the initial QC flags
  save(otg_raw,
       file = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/",
                     yw,
                     "/1_formatted_csvs/otg_raw.rda"))

  rm(otg_raw, path)

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import raw, QC, and save loop

#-----------------------------
# LOOP 1.5: re-import "QC" .csvs, fix common mistakes w/ below code, overwrite .csvs
#-----------------------------
# We've identified a number of common errors in the 2019/2020 data. For example, many of
# the errors in the channel unit data, especially for 2019/lemhi, are related to ocular
# substrate estimates not summing to 100 and fish cover estimates not summing to within an
# expected range. Let's resolve those common errors using some functions we've created
# within the DASH package.

# when running this code, only run the loop below once (set r_code_fixes = TRUE)
r_code_fixes = FALSE

if(r_code_fixes) {
  for (yw in yr_wtsd) {

    # set path for yr_wtsd
    path_format = paste0(nas_prefix,
                         "/data/habitat/DASH/OTG/",
                         yw,
                         "/1_formatted_csvs/")

    # set path for yr_wtsd, except QC'd data
    path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/",
                  yw,
                  "/2_qcd_csvs/")

    otg_temp = read_otg_csv_wrapper(path = path,
                                    otg_type_list = c("surveyPoint_0.csv",
                                                      "CU_1.csv",
                                                      "Wood_2.csv",
                                                      "Jam_3.csv",
                                                      "Undercut_4.csv",
                                                      "Discharge_5.csv",
                                                      "DischargeMeasurements_6.csv"),
                                    otg_type_names = c("survey",
                                                       "cu",
                                                       "wood",
                                                       "jam",
                                                       "undercut",
                                                       "discharge",
                                                       "discharge_measurements"))

    # ocular substrate fixes
    otg_temp$cu = rescale_values(data_df = otg_temp$cu,
                                 col_names = c("Sand/Fines 2mm",
                                               "Gravel 2-64mm",
                                               "Cobble 64-256mm",
                                               "Boulder 256mm"),
                                 min_value = 90,
                                 max_value = 110,
                                 sum_to = 100)

    # fish cover fixes
    otg_temp$cu = fix_fish_cover(cu_df = otg_temp$cu,
                                 cover_cols = c("Overhanging Cover",
                                                "Aquatic Vegetation",
                                                "Woody Debris Cover",
                                                "Artificial Cover",
                                                "Total No Cover"))

    # Additionally, we need to make discharge measurements consistent
    # year-to-year. Particularly, what the station width measurement
    # actually represents.

    # make 2019 discharge measurement station_width's match 2020 format
    if(grepl("2019", yw)) {
      otg_temp$discharge_measurements = otg_temp$discharge_measurements %>%
        group_by(ParentGlobalID) %>%
        mutate(width_lag = lag(`Station Width`),
               `Station Width` = if_else(is.na(width_lag),
                                         `Station Width`,
                                         `Station Width` - width_lag),
               `Station Width` = round(`Station Width`, 4)) %>%
        select(-width_lag) %>%
        ungroup()
    }

    # for 2020, add a measurement at the short bank
    if(grepl("2020", yw)) {
      otg_temp$discharge_measurements = otg_temp$discharge_measurements %>%
        group_by(ParentGlobalID) %>%
        # determine which is the first and last station at a site
        mutate(station = 1:n()) %>%
        group_split() %>%
        map(.f = function(x) {
          if(x$`Station Width`[x$station == 1] > 0) {
            y = x %>%
              bind_rows(x %>%
                          filter(station == 1) %>%
                          mutate(ObjectID = NA,
                                 GlobalID = NA,
                                 `Station Width` = 0,
                                 `Station Depth` = 0,
                                 `Station Velocity` = 0,
                                 station = 0)) %>%
              arrange(station)
            return(y)
          } else {
            return(x)
          }
        }) %>%
        map_df(.f = identity) %>%
        select(-station) %>%
        ungroup()
    }

    # overwrite csvs with updated values
    otg_temp %>%
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
              write_csv(paste0(path, unique(y$path_nm)))
            # return(NULL)
          })
        # return(NULL)
      })

    # after last loop, sound an alarm
    if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

  } # end coded "automatic" fixes
}

#-----------------------------
# LOOP 2: iterative loop: import "QC" .csvs, run QC, write QC results,
# fix problems in said "QC" .csvs, rinse and repeat...
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd, except QC'd data
  path = paste0(nas_prefix,
                "/data/habitat/DASH/OTG/",
                yw,
                "/2_qcd_csvs/")

  otg_interim = read_otg_csv_wrapper(path = path,
                                     otg_type_list = c("surveyPoint_0.csv",
                                                       "CU_1.csv",
                                                       "Wood_2.csv",
                                                       "Jam_3.csv",
                                                       "Undercut_4.csv",
                                                       "Discharge_5.csv",
                                                       "DischargeMeasurements_6.csv"),
                                     otg_type_names = c("survey",
                                                        "cu",
                                                        "wood",
                                                        "jam",
                                                        "undercut",
                                                        "discharge",
                                                        "discharge_measurements"))

  # perform QC on the qc'd data
  qc_interim = qc_wrapper(survey_df = otg_interim$survey,
                          cu_df = otg_interim$cu,
                          wood_df = otg_interim$wood,
                          jam_df = otg_interim$jam,
                          undercut_df = otg_interim$undercut,
                          discharge_df = otg_interim$discharge,
                          discharge_meas_df = otg_interim$discharge_measurements,
                          channel_unit_roll_qc = T)

  # write interim QC results
  write_csv(qc_interim, paste0(path,
                               "qc_interim_",
                               format(Sys.Date(), format = "%Y%m%d"),
                               ".csv"))

  # save the otg_interim list of dfs
  save(otg_interim,
       file = paste0(path,
                     "otg_interim.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end interim QC loop

# IMPORTANT: THE ABOVE LOOP 1) IMPORTING QC'D DATA AND QC'ING THAT DATA IS A
# POTENTIALLY ITERATIVE PROCESS, DESCRIBED BELOW

# At this point, someone very familiar with the OTG data, (preferably a field technician or field coordinator,
# secondarily a project leader) should intervene, review the QC errors flagged on the /2_qcd_csvs/ versions of
# the data that we just wrote out, attempt to resolve those, and ideally, make notes for those QC errors that can't
# be resolved. Errors should be resolved to the .csvs within the survey folders in the /2_qcd_csvs/ directory.

# In addition, for the QC errors that are resolved, it is useful to provide notes on how they are resolved. Those
# notes on how errors were/were not resolved care helpful towards improving potential data validation (i.e.,
# validation of data during field surveys) or quality control steps in the future.

# After resolving QC's to the best of your abilities, the next step is to re-run the above loop. This will
# re-import the .csvs with the "improved" data and re-run the QC, exporting a new summary of remaining flags,
# which can then be reviewed again. This process can be repeated until as many errors are resolved as possible.
# It is fine to add notes in the above "qc_final_YYYYMMDD.csv" data about how errors were or were not resolved.

#-----------------------------
# LOOP 3: re-import "QC" .csvs, perform final QC
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd, except QC'd data
  path = paste0(nas_prefix,
                "/data/habitat/DASH/OTG/",
                yw,
                "/2_qcd_csvs/")

  otg_qcd = read_otg_csv_wrapper(path = path,
                                 otg_type_list = c("surveyPoint_0.csv",
                                                   "CU_1.csv",
                                                   "Wood_2.csv",
                                                   "Jam_3.csv",
                                                   "Undercut_4.csv",
                                                   "Discharge_5.csv",
                                                   "DischargeMeasurements_6.csv"),
                                 otg_type_names = c("survey",
                                                    "cu",
                                                    "wood",
                                                    "jam",
                                                    "undercut",
                                                    "discharge",
                                                    "discharge_measurements"))

  # perform QC on the "final" OTG data
  qc_final = qc_wrapper(survey_df = otg_qcd$survey,
                        cu_df = otg_qcd$cu,
                        wood_df = otg_qcd$wood,
                        jam_df = otg_qcd$jam,
                        undercut_df = otg_qcd$undercut,
                        discharge_df = otg_qcd$discharge,
                        discharge_meas_df = otg_qcd$discharge_measurements,
                        channel_unit_roll_qc = T)

  # write "final" QC results
  otg_qcd$qc_results = qc_final

  # save as .Rdata object
  save(otg_qcd,
       file = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/",
                     yw,
                     "/prepped/otg_qcd.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import QC'd data, record final QC messages, and export prepped data

# END SCRIPT
