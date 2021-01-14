# Authors: Mike Ackerman
#
# Purpose: A script to import and QC all of the 2019 & 2020 on-the-ground (OTG)
# DASH data collected using Survey123.
#
# Created: July 15, 2020
#   Last Modified: December 9, 2020
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
                          discharge_meas_df = otg_raw$discharge_measurements)

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

  # Many errors in the "CU" data, especially for 2019/lemhi are related to ocular substrate
  # estimates not summing to 100 and fish cover estimates not summing to within an
  # expected range. Let's resolve those using some functions we've created within the
  # DASH package.

  # I've also identified a handful of the same errors in the nf_salmon 2019 &
  # secesh 2020 data. So I'll expand this loop here to include those.

  # 1/14/2021: We've also identified a handful of the same errors in the nf_salmon 2019 &
  # secesh 2020 data. We've expanded this if loop to include those.
  if( yw == "2019/lemhi" | yw == "2019/nf_salmon" | yw == "2020/secesh" ) {

    # ocular substrate estimates
    qc_results %>%
      filter(source == "CU") %>%
      filter(grepl("Ocular estimates sum", error_message)) %>%
      left_join(otg_raw$cu) %>%
      select(source:GlobalID,
             `Channel Unit Type`,
             `Sand/Fines 2mm`:`Boulder 256mm`) %>%
      mutate(oc_cover = rowSums(.[5:8], na.rm = T))

    # resolve ocular substrate estimates that sum close to 100, but not quite
    otg_raw$cu = rescale_values(data_df = otg_raw$cu,
                                col_names = c("Sand/Fines 2mm",
                                              "Gravel 2-64mm",
                                              "Cobble 64-256mm",
                                              "Boulder 256mm"),
                                min_value = 90,
                                max_value = 110,
                                sum_to = 100)

    # fish cover estimates
    qc_results %>%
      filter(source == "CU") %>%
      filter(grepl("Cover values sum", error_message)) %>%
      left_join(otg_raw$cu) %>%
      select(source:GlobalID,
             `Overhanging Cover`:`Total No Cover`) %>%
      mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
      # select(path_nm, sum_cover) %>%
      # table()
      tabyl(sum_cover)

    # lots of issues w/ fish cover ests, resolve some...
    otg_raw$cu = fix_fish_cover(cu_df = otg_raw$cu,
                                cover_cols = c("Overhanging Cover",
                                               "Aquatic Vegetation",
                                               "Woody Debris Cover",
                                               "Artificial Cover",
                                               "Total No Cover"))

    # re-run QC after resolving above issues
    qc_results = qc_wrapper(survey_df = otg_raw$survey,
                            cu_df = otg_raw$cu,
                            wood_df = otg_raw$wood,
                            jam_df = otg_raw$jam,
                            undercut_df = otg_raw$undercut,
                            discharge_df = otg_raw$discharge,
                            discharge_meas_df = otg_raw$discharge_measurements)

  } # end if yw = "2019/lemhi" loop

  # save QC results
  qc_init_path = paste0(nas_prefix,
                        "/data/habitat/DASH/OTG/",
                        yw,
                        "/1_formatted_csvs/qc_results")
  save(qc_results, file = paste0(qc_init_path, ".rds"))
  write_csv(qc_results, paste0(qc_init_path,
                               "_",
                               format(Sys.Date(), format = "%Y%m%d"),
                               ".csv"))

} # end QC loop

# At this point, someone very familiar with the OTG data (preferably a field technician or field coordinator,
# secondarily a project leader) should likely intervene, review the remaining QC errors that we just wrote
# to file, and attempt to resolve those, and ideally, make notes for those QC errors that can't be resolved.
# In addition, for the QC errors that are resolved, it is useful to provide notes on how they are resolved. Notes
# on how errors were or were not resolved can be useful towards improving data validation (e.g., during field
# collections) or quality control steps in the future.

# Above, we imported data from the "/1_formatted_csvs/" directory for 3 year x watershed combinations ("2019/lemhi",
# "2019/nf_salmon", and "2020/secesh"), and in the case of "2019/lemhi" resolved some common issues in ocular
# estimates using the rescale_values() function and fish cover estimates using fix_fish_cover(), and then wrote out all
# of the remaining identified errors to a file in each respective directory titled "qc_results_YYYYMMDD.csv" which
# is the file that should be reviewed.

# The next suggested step is to copy/paste all of the Survey123 data to a new directory for each year x watershed
# combination; we used "/2_qcd_csvs/". And then, only data in the "/2_qcd_csvs/" directories should be modified while
# reviewing the QC results. Doing so preserves the integrity of the raw data in "/1_formatted_csvs/", which is good
# general practice. However, it is fine to add notes in the "qc_results_YYYYMMDD.csv" about how errors were or were
# not resolved (those qc results can always be replicated in the future using this script).

# Note: the data in the "/1_formatted_csvs/" directory and "/0_raw_csvs/" directories only differ in that file
# formatting issues may have been identified during data import (i.e., no data has changed).

# Now that errors have been resolved (which as of 20201209 they all have not, still need review), we can move on and
# re-import the "/2_qcd_csvs/" data in which some errors have been resolved.

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

  # recall, for the lemhi 2019 data, we need to re-do our fixes to the ocular substrate
  # and fish cover estimates

  # I've also identified a handful of the same errors in the nf_salmon 2019 &
  # secesh 2020 data. So I'll expand this loop here to include those.
  if( yw == "2019/lemhi" | yw == "2019/nf_salmon" | yw == "2020/secesh" ) {

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

  } # end if loop

  # we need to make the discharge measurements consistent year-to-year
  # particularly what station width represents
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

  # in 2020, add a measurement at the short bank
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
    map(.f = function(x) {
      x %>%
        group_by(path_nm) %>%
        group_split() %>%
        map(.f = function(y) {
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
                        discharge_meas_df = otg_qcd$discharge_measurements)

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

# Here, we write out the remaining QC results to both files "qc_final.rds" and "qc_final_YYYYMMDD.csv"
# in the "/2_qcd_csvs/" directory for each year x watershed combo. These are those remaining QC errors
# that just can't be resolved.

#-----------------------------
# Load, join, clean, and rollup QC'd data to the channel unit scale
#-----------------------------
otg_path = paste0(nas_prefix,
                  "/data/habitat/DASH/OTG/")

# list of otg_qcd files in otg_path
otg_qcd_files = list.files(path = otg_path,
                           pattern = "^otg_qcd\\.rda$",
                           recursive = T) %>%
  paste0(otg_path, .)

# load all files, and join them together
otg_list = as.list(otg_qcd_files) %>%
  map(.f = function(x) {
    load(x) %>%
      get() %>%
      map(clean_names)
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

# clean up
rm(otg_path,
   otg_qcd_files,
   otg_list)

#-----------------------------
# clean cu data and join site info to it
#-----------------------------
# clean survey names
otg$survey = otg$survey %>%
  mutate(across(site_name,
                ~ str_replace(., "_.*", "")),
         across(site_name,
                ~str_remove(., " ")))

# # make 2019 discharge measurement station_width's match 2020 format
# if(grepl("2019$", otg_path)) {
#   otg_all$discharge_measurements = otg_all$discharge_measurements %>%
#     group_by(parent_global_id) %>%
#     mutate(width_lag = lag(station_width),
#            station_width = if_else(is.na(width_lag),
#                                    station_width,
#                                    station_width - width_lag)) %>%
#     select(-width_lag)
# }
#
# # in 2020, add a measurement at the short bank
# if(grepl("2020$", otg_path)) {
#   otg_all$discharge_measurements = otg_all$discharge_measurements %>%
#     group_by(parent_global_id) %>%
#     # determine which is the first and last station at a site
#     mutate(station = 1:n()) %>%
#     group_split() %>%
#     map(.f = function(x) {
#       if(x$station_width[x$station == 1] > 0) {
#         y = x %>%
#           bind_rows(x %>%
#                       filter(station == 1) %>%
#                       mutate(object_id = NA,
#                              global_id = NA,
#                              station_width = 0,
#                              station_depth = 0,
#                              station_velocity = 0,
#                              station = 0)) %>%
#           arrange(station)
#         return(y)
#       } else {
#         return(x)
#       }
#     }) %>%
#     map_df(.f = identity)
# }

# CU
cu_cu = rollup_cu(cu_df = otg$cu,
                  survey_df = otg$survey)

#-----------------------------
# start rolling up data to cu scale
#-----------------------------
cu_wood = rollup_cu_wood(wood_df = otg$wood)
cu_jam = rollup_cu_jam(jam_df = otg$jam)
cu_undercut = rollup_cu_undercut(undercut_df = otg$undercut)

# discharge is a little tricky
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

# there's an error here somewhere in the Secesh discharge data, rollup, something
# Also need to resolve the BOS and TOS issue. How do I assign a CU number to the BOS and TOS discharge measurements?

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
     file = paste0(nas_prefix, "data/habitat/DASH/OTG/prepped/dash_2019_2020_otg_cu.rda"))

write_csv(dash_otg_cu,
          paste0(nas_prefix, "data/habitat/DASH/OTG/prepped/dash_2019_2020_otg_cu.csv"))

### END SCRIPT
