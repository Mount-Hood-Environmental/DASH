# Authors: Mike Ackerman
#
# Purpose: A script to import and QC all of the 2019 & 2020 on-the-ground (OTG)
# DASH data collected using Survey123.
#
# Created: July 15, 2020
#   Last Modified: November 5, 2020
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
library(beepr)
#library(tidyverse)
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
# set some arguments/parameters
#-----------------------------
# create a vector to directories containing the raw data, formatted for import
yr_wtsd = c("2019/lemhi",
            "2019/nf_salmon",
            "2020/secesh")

#-----------------------------
# read in OTG data; loop over year_watershed combinations
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
# compare raw *unformatted* data to raw *formatted* data
# move this chunk to QC script at a later time
#-----------------------------
for (yw in yr_wtsd) {

  # set the directories to compare
  path1 = paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/",
                 yw,
                 "/0_raw_csvs/")
  path2 = paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/",
                 yw,
                 "/1_formatted_csvs/")

  tmp = compare_folders(path1 = path1,
                        path2 = path2)

  assign(paste0("unf_vs_for_",
                gsub("/", "", yw)),
         tmp)
  rm(tmp)

}

# EXAMPLES: examine differences in files
# exploring would be useful for looking at issues in survey123 forms
unf_vs_for_2019lemhi$diff_files
unf_vs_for_2019lemhi$differences$`Canyon_Survey123_2019/Undercut_4.csv`

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
                          disch_meas_df = otg_raw$discharge_measurements)

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
  if( yw == "2019/lemhi" ) {

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
      select(path_nm, sum_cover) %>%
      table()

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
                            disch_meas_df = otg_raw$discharge_measurements)

  } # end if yw = "2019/lemhi" loop

  # save QC results
  qc_path = paste0(nas_prefix,
                   "data/habitat/DASH/OTG/",
                   yw,
                   "/1_formatted_csvs/qc_results")
  save(qc_results, file = paste0(qc_path, ".rds"))
  write_csv(qc_results, paste0(qc_path,
                               "_",
                               format(Sys.Date(), format = "%Y%m%d"),
                               ".csv"))

} # end QC loop

# At this point, someone very familiar with the OTG data (field technician, field coordinator)
# should likely intervene, review the remaining QC errors that we just wrote out, attempt to resolve those,
# and ideally make notes for those that can't be resolved, and for those than can, identify how
# the issue was resolved. Changes to any data should be made to a copy of all of the data i.e.,
# do not modify the raw data!

#######################################################################################
