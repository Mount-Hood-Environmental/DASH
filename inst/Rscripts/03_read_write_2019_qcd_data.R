# Authors: Mike Ackerman & Kevin See
#
# Purpose: After resolving issues in the raw data identified
# during the QC process, we need to re-import that "QC'd" data,
# make some changes, and save the "final" data as a .rda object
#
# Created: October 2, 2020
#   Last Modified:
#
# Notes:
rm(list = ls())

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS"
}

#-----------------------------
# Import "QC'd" data
#-----------------------------
trib = "lemhi"
#trib = "nf_salmon"

path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/2_qcd_csvs/")
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
beepr::beep(sound = "ping")

#-----------------------------
# Recall, we need to re-do our fixes to the ocular substrate and fish cover estimates...
#-----------------------------
otg_qcd$cu = rescale_values(data_df = otg_qcd$cu,
                            col_names = c("Sand/Fines 2mm",
                                          "Gravel 2-64mm",
                                          "Cobble 64-256mm",
                                          "Boulder 256mm"),
                            min_value = 90,
                            max_value = 110,
                            sum_to = 100)

otg_qcd$cu = fix_fish_cover(cu_df = otg_qcd$cu,
                            cover_cols = c("Overhanging Cover",
                                           "Aquatic Vegetation",
                                           "Woody Debris Cover",
                                           "Artificial Cover",
                                           "Total No Cover"))

# perform QC on the qc'd data
qc_final = qc_wrapper(survey_df = otg_qcd$survey,
                      cu_df = otg_qcd$cu,
                      wood_df = otg_qcd$wood,
                      jam_df = otg_qcd$jam,
                      undercut_df = otg_qcd$undercut,
                      discharge_df = otg_qcd$discharge,
                      disch_meas_df = otg_qcd$discharge_measurements)

# Write out remaining QC results
output_path = paste0(nas_prefix,
                     "/data/habitat/DASH/OTG/2019/", trib, "/2_qcd_csvs/final_qc_DASH_2019_",
                     format(Sys.Date(), format = "%Y%m%d"),
                     ".csv")
readr::write_csv(qc_final, output_path)

#-----------------------------
# Save the QC'd data
#-----------------------------
save(otg_qcd,
     file = paste0(nas_prefix,"/data/habitat/DASH/OTG/2019/", trib, "/prepped/qcd_DASH_2019_otg.rda"))

#-----------------------------
# Finally, we can save "manual" changes to the raw DASH data
#-----------------------------
# the directories to compare
path1 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/1_formatted_csvs/")
path2 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/", trib, "/2_qcd_csvs/")

# use compare_folders()
raw_vs_qcd = compare_folders(path1 = path1,
                             path2 = path2)
save(raw_vs_qcd,
     file = paste0(nas_prefix, "data/habitat/DASH/OTG/2019/", trib, "/prepped/qc_resolutions.rda"))

### END SCRIPT
