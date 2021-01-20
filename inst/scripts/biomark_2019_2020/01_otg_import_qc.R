# Authors: Mike Ackerman
#
# Purpose: A script to import and QC all of the 2019 & 2020 on-the-ground (OTG)
# DASH data collected using Survey123.
#
# Created: July 15, 2020
#   Last Modified: January 20, 2021
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
# import raw OTG data; loop over year_watershed combinations
#-----------------------------
for (yw in yr_wtsd) {

  # set path for yr_wtsd
  path = paste0(nas_prefix,
                "/data/habitat/DASH/OTG/",
                yw,
                "/1_formatted_csvs/")

  # loop over OTG data types using the wrapper function
  otg_raw = read_otg_csv_wrapper(path = path,
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

  # EXAMPLE: read in just one type of OTG data
  # otg_raw_cu = read_otg_csv(path,
  #                           otg_type = "CU_1.csv")

  # save the otg_data list of dfs
  save(otg_raw,
       file = paste0(nas_prefix,
                     "data/habitat/DASH/OTG/",
                     yw,
                     "/prepped/otg_raw.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import raw and save loop

#-----------------------------
# Initial data QC
#-----------------------------
for (yw in yr_wtsd) {

  load(paste0(nas_prefix,
              "/data/habitat/DASH/OTG/",
              yw,
              "/prepped/otg_raw.rda"))

  # QC all data types for each yr_wtsd
  qc_results = qc_wrapper(survey_df = otg_raw$survey,
                          cu_df = otg_raw$cu,
                          wood_df = otg_raw$wood,
                          jam_df = otg_raw$jam,
                          undercut_df = otg_raw$undercut,
                          discharge_df = otg_raw$discharge,
                          discharge_meas_df = otg_raw$discharge_measurements,
                          redirect_output = F)

  #-----------------------------
  # ALTERNATIVE EXAMPLES: QC just a couple otg_types, separately
  #-----------------------------
  # qc_results_cu = qc_cu(qc_df = otg_raw$cu)
  # qc_results_wood = qc_wood(qc_df = otg_raw$wood)
  #
  # qc_results_some = qc_results_cu %>%
  #   tibble::add_column(source = "CU",
  #                      .before = 0) %>%
  #   bind_rows(qc_results_wood %>%
  #               tibble::add_column(source = "Wood",
  #                                  .before = 0))
  #
  # qc_results_some = qc_wrapper(cu_df = otg_raw$cu,
  #                              wood_df = otg_raw$wood)

  # save initial QC results
  qc_init_path = paste0(nas_prefix,
                        "/data/habitat/DASH/OTG/",
                        yw,
                        "/1_formatted_csvs/qc_results")
  save(qc_results, file = paste0(qc_init_path, ".rds"))
  write_csv(qc_results, paste0(qc_init_path,
                               "_",
                               format(Sys.Date(), format = "%Y%m%d"),
                               ".csv"))

} # end initial QC loop

#-----------------------------
# import QC'd OTG data; loop over year_watershed combinations
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

  # We've identified a number of common errors in the 2019/2020 data. For example, many of
  # the errors in the channel unit data, especially for 2019/lemhi, are related to ocular
  # substrate estimates not summing to 100 and fish cover estimates not summing to within an
  # expected range. Let's resolve those common errors using some functions we've created
  # within the DASH package.

  # ocular substrate fixes
  otg_qcd$cu = rescale_values(data_df = otg_qcd$cu,
                              col_names = c("Sand/Fines 2mm",
                                            "Gravel 2-64mm",
                                            "Cobble 64-256mm",
                                            "Boulder 256mm"),
                              min_value = 90,
                              max_value = 110,
                              sum_to = 100)

  # fish cover fixes
  otg_qcd$cu = fix_fish_cover(cu_df = otg_qcd$cu,
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
    otg_qcd$discharge_measurements = otg_qcd$discharge_measurements %>%
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
    otg_qcd$discharge_measurements = otg_qcd$discharge_measurements %>%
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
  otg_qcd %>%
    walk(.f = function(x) {
      # fix issue with writing date from survey back to csv
      if("Survey Date" %in% names(x)) {
        x = x %>%
          mutate(`Survey Date` = map_chr(path_nm,
                                    .f = function(y) {
                                      suppressMessages(read_csv(paste0(path, y))) %>%
                                        pull(`Survey Date`)
                                    }))
      }

      if(sum(c("CreationDate", "EditDate") %in% names(x)) > 0) {
        x = x %>%
          mutate(across(c(CreationDate, EditDate),
                        ~ format(., "%m/%d/%Y")))
      }

      x %>%
        group_by(path_nm) %>%
        group_split() %>%
        walk(.f = function(y) {
          y %>%
            select(-path_nm) %>%
            write_csv(paste0(path, unique(y$path_nm)))
        })
    })

  # save as .Rdata object
  save(otg_qcd,
       file = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/",
                     yw,
                     "/prepped/otg_qcd.rda"))

  # after last loop, sound an alarm
  if (yw == tail(yr_wtsd, 1)) { beepr::beep(3) }

} # end import QC'd data and save loop

#-----------------------------
# "Final" data QC
#-----------------------------
for (yw in yr_wtsd) {

  # load QC'd data
  load(paste0(nas_prefix,
              "/data/habitat/DASH/OTG/",
              yw,
              "/prepped/otg_qcd.rda"))

  # perform QC on the qc'd data
  qc_final = qc_wrapper(survey_df = otg_qcd$survey,
                        cu_df = otg_qcd$cu,
                        wood_df = otg_qcd$wood,
                        jam_df = otg_qcd$jam,
                        undercut_df = otg_qcd$undercut,
                        discharge_df = otg_qcd$discharge,
                        discharge_meas_df = otg_qcd$discharge_measurements,
                        redirect_output = F)

  qc_final_path = paste0(nas_prefix,
                         "/data/habitat/DASH/OTG/",
                         yw,
                         "/2_qcd_csvs/qc_final")
  save(qc_final, file = paste0(qc_final_path, ".rds"))
  write_csv(qc_final, paste0(qc_final_path,
                             "_",
                             format(Sys.Date(), format = "%Y%m%d"),
                             ".csv"))

} # end qc final data loop

# IMPORTANT: THE ABOVE TWO LOOPS 1) IMPORTING QC'D DATA AND QC'ING THAT DATA IS A
# POTENTIALLY ITERATIVE PROCESS, DESCRIBED BELOW

# At this point, someone very familiar with the OTG data, (preferably a field technician or field coordinator,
# secondarily a project leader) should intervene, review the QC errors flagged on the /2_qcd_csvs/ versions of
# the data that we just wrote out, attempt to resolve those, and ideally, make notes for those QC errors that can't
# be resolved. Errors should be resolved to the .csvs within the survey folders in the /2_qcd_csvs/ directory.

# In addition, for the QC errors that are resolved, it is useful to provide notes on how they are resolved. Those
# notes on how errors were/were not resolved care helpful towards improving potential data validation (i.e.,
# validation of data during field surveys) or quality control steps in the future.

# After resolving QC's to the best of your abilities, the next step is to re-run the above two loops. This will
# re-import the .csvs with the "improved" data and re-run the QC, exporting a new summary of remaining flags,
# which can then be reviewed again. This process can be repeated until as many errors are resolved as possible.
# It is fine to add notes in the above "qc_final_YYYYMMDD.csv" data about how errors were or were not resolved.

# Now that errors have been resolved (which as on 20210120 they all have not, still need to review), we can proceed.
# As of 20210120, the majority of QC flags in the "2019/lemhi" data have been reviewed, but those in the
# "2019/nf_salmon", "2020/lemhi", and "2020/secesh" have not.

# END SCRIPT

