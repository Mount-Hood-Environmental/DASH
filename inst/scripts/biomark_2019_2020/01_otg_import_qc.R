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
  qc_raw_tmp = qc_wrapper(survey_df = otg_raw$survey,
                          cu_df = otg_raw$cu,
                          wood_df = otg_raw$wood,
                          jam_df = otg_raw$jam,
                          undercut_df = otg_raw$undercut,
                          discharge_df = otg_raw$discharge,
                          disch_meas_df = otg_raw$discharge_measurements)

  #-----------------------------
  # ALTERNATIVE EXAMPLES: QC just a couple otg_types, separately
  #-----------------------------
  # qc_raw_cu = qc_cu(qc_df = otg_raw$cu)
  # qc_raw_wood = qc_wood(qc_df = otg_raw$wood)
  #
  # qc_raw_some = qc_raw_cu %>%
  #   tibble::add_column(source = "CU",
  #                      .before = 0) %>%
  #   bind_rows(qc_raw_wood %>%
  #               tibble::add_column(source = "Wood",
  #                                  .before = 0))
  #
  # qc_raw_some = qc_wrapper(cu_df = otg_raw$cu,
  #                          wood_df = otg_raw$wood)

  assign(paste0("qc_raw_",
                gsub("/", "", yw)),
         qc_raw_tmp)
  rm(qc_raw_tmp)

} # end QC loop



#######################################################################################
#-----------------------------
# Quick look at the initial QC results
#-----------------------------
# where do all the QA/QC errors come from?
janitor::tabyl(qc_raw, source)

# a large chunk are from the "CU" data, examine a little further...
qc_raw %>%
  filter(source == "CU") %>%
  count(error_message) %>%
  print(n = 100)

# Note many errors in the "CU" data are related to ocular substrate estimates not summing to 100
# and fish cover estimates not summing to within an expected range. Let's resolve those using
# some functions we've created within the DASH package

# Ocular substrate estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# couple ocular substrate estimates that sum to 90, resolve using rescale_values()
otg_raw$cu = rescale_values(data_df = otg_raw$cu,
                            col_names = c("Sand/Fines 2mm",
                                          "Gravel 2-64mm",
                                          "Cobble 64-256mm",
                                          "Boulder 256mm"),
                            min_value = 90,
                            max_value = 110,
                            sum_to = 100)

# Fish cover estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  select(path_nm, sum_cover) %>%
  #select(sum_cover) %>%
  table()

# lots of issues with fish cover estimates, resolve some using fix_fish_cover()...
otg_raw$cu = fix_fish_cover(cu_df = otg_raw$cu,
                            cover_cols = c("Overhanging Cover",
                                           "Aquatic Vegetation",
                                           "Woody Debris Cover",
                                           "Artificial Cover",
                                           "Total No Cover"))

#-----------------------------
# Re-run the QC after resolving some ocular and cover issues
#-----------------------------
qc_raw = qc_wrapper(survey_df = otg_raw$survey,
                    cu_df = otg_raw$cu,
                    wood_df = otg_raw$wood,
                    jam_df = otg_raw$jam,
                    undercut_df = otg_raw$undercut,
                    discharge_df = otg_raw$discharge,
                    disch_meas_df = otg_raw$discharge_measurements)

#-----------------------------
# Examine the remaining QC errors in R
#-----------------------------
# latitude, longitude errors
qc_raw %>%
  filter(source == "Survey") %>%
  left_join(otg_raw$survey) %>%
  select(source:error_message, x:y)

# `Maximum Depth (m)` errors
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Column Maximum Depth", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:error_message, `Channel Unit Type`, `Maximum Depth (m)`) %>%
  #View()
  janitor::tabyl(`Channel Unit Type`)

# Duplicate channel units
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("appears more than once", error_message)) #%>%
#View()

# Thalweg exit depth
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Thalweg exit depth", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:error_message,
         `Channel Unit Number`,
         `Channel Segment Number`,
         `Thalweg Exit Depth (m)`)

# Ocular substrate estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Ocular estimates sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Channel Unit Type`,
         `Sand/Fines 2mm`:`Boulder 256mm`) %>%
  mutate(oc_cover = rowSums(.[5:8], na.rm = T))

# Cover estimates
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Cover values sum", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(source:GlobalID,
         `Overhanging Cover`:`Total No Cover`) %>%
  mutate(sum_cover = rowSums(.[4:8], na.rm = T)) %>%
  #select(path_nm, sum_cover) %>%
  select(sum_cover) %>%
  table()

# Pebble counts
qc_raw %>%
  filter(source == "CU") %>%
  filter(grepl("Pebble size values", error_message)) %>%
  left_join(otg_raw$cu) %>%
  select(path_nm:GlobalID,
         contains("Pebble")) #%>%
#View()

# Wood
qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Large Wood Number is <blank> or NA.", error_message)) %>%
  left_join(otg_raw$wood)

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Column Diameter", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         ObjectID:`Ballasted?`) #%>%
#View()

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Wet|Channel Forming|Ballasted", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         `Wet?`,
         `Channel Forming?`,
         `Ballasted?`) %>%
  pivot_longer(`Wet?`:`Ballasted?`) %>%
  filter(is.na(value)) #%>%
#View()

qc_raw %>%
  filter(source == "Wood") %>%
  filter(grepl("Length is less than or equal to the diameter|falls outside of the expected", error_message)) %>%
  left_join(otg_raw$wood) %>%
  select(path_nm:GlobalID,
         `Length (m)`,
         `Diameter (m)`)

# Jam
qc_raw %>%
  filter(source == "Jam") %>%
  left_join(otg_raw$jam) %>%
  select(-(ParentGlobalID:Editor)) %>%
  as.data.frame() #%>%
#View()

# Undercut
qc_raw %>%
  filter(source == "Undercut") %>%
  left_join(otg_raw$undercut) %>%
  select(path_nm:error_message,
         `Undercut Number`:`Width 75% (m)`) %>%
  as.data.frame() #%>%
#View()

# Discharge measurements
qc_raw %>%
  filter(source == "DischargeMeasurements") %>%
  left_join(otg_raw$discharge_measurements) %>%
  select(path_nm:error_message,
         ObjectID:`Station Velocity`) %>%
  as.data.frame() #%>%
#View()

# write out remaining QC error messages
output_path = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/2019/", trib, "/1_formatted_csvs/qc_results_DASH_2019_",
                     format(Sys.Date(), format = "%Y%m%d"),
                     ".csv")
readr::write_csv(qc_raw, output_path)

# At this point, someone very familiar with the OTG data (field technician, field coordinator)
# should likely intervene, review the remaining QC errors that we just wrote out, attempt to resolve those,
# and ideally make notes for those that can't be resolved, and for those than can, identify how
# the issue was resolved. Changes to any data should be made to a copy of all of the data i.e.,
# do not modify the raw data!

# END SCRIPT
