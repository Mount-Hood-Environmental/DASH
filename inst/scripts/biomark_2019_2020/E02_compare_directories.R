# Authors: Mike Ackerman
#
# Purpose: A script to find differences in files within
# directories. Useful for exploring formatting issues
# that were resolved during the data import process and/or
# looking at changes in the data made during the QC
# process
#
# Created: November 6, 2020
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

#-----------------------------
# load necessary libraries
#-----------------------------
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
# compare raw *unformatted* data to raw *formatted* data
#-----------------------------
yr_wtsd = c("2019/lemhi",
            "2019/nf_salmon",
            "2020/secesh")

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

} # end for loop

# EXAMPLES: examine differences in files
# exploring would be useful for looking at issues in survey123 forms
unf_vs_for_2019lemhi$diff_files
unf_vs_for_2019lemhi$differences$`Canyon_Survey123_2019/Undercut_4.csv`
unf_vs_for_2019lemhi$differences[[paste0(unf_vs_for_2019lemhi$diff_files[3])]]

#-----------------------------
# compare raw data to QC'd data and save "manual" changes
#-----------------------------
# useful for reviewing "manual" changes to the data that someone made after reviewing QC results
for (yw in yr_wtsd) {

  # set the directories to compare
  path1 = paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/",
                 yw,
                 "/1_formatted_csvs/")
  path2 = paste0(nas_prefix,
                 "/data/habitat/DASH/OTG/",
                 yw,
                 "/2_qcd_csvs/")

  raw_vs_qc = compare_folders(path1 = path1,
                              path2 = path2)

  save(raw_vs_qc,
       file = paste0(nas_prefix,
                     "data/habitat/DASH/OTG/",
                     yw,
                     "/2_qcd_csvs/qc_data_changes.rda"))

} # end compare raw vs QC loop

#-----------------------------
# now pull out actual differences in metrics in an easier format to examine
#-----------------------------
comp_list = vector("list", length(raw_vs_qc$diff_files))
for(i in 1:length(comp_list)) {
  file1 = paste0(path1, raw_vs_qc$diff_files[i])
  file2 = paste0(path2, raw_vs_qc$diff_files[i])

  comp_list[[i]] = compare_files(file1, file2)

}

for(i in 1:length(comp_list)) {
  if(i == 1) {
    all_comps = comp_list[[1]]
  } else {
    all_comps = suppressMessages(purrr::map2(all_comps,
                                             comp_list[[i]],
                                             dplyr::full_join))
  }
}

# differences in character or factor metrics
all_comps$chr_diff

all_comps$chr_diff %>%
  # inner_join(otg_raw_all$cu)
  inner_join(otg$cu)

# differences in numeric metrics
tabyl(all_comps$num_diff, source)



all_comps$num_diff %>%
  filter(source == "DischargeMeasurements") %>%
  # inner_join(otg_raw_all$discharge_measurements)
  inner_join(otg$discharge_measurements)

all_comps$num_diff %>%
  filter(source == "Jam") %>%
  # inner_join(otg_raw_all$jam)
  inner_join(otg$jam)

all_comps$num_diff %>%
  filter(source == "Undercut") %>%
  # inner_join(otg_raw_all$undercut)
  inner_join(otg$undercut)

all_comps$num_diff %>%
  filter(source == "Wood") %>%
  # inner_join(otg_raw_all$wood)
  inner_join(otg$wood)

all_comps$num_diff %>%
  filter(source == "CU") %>%
  # filter(is.na(file2)) %>%
  # select(global_id) %>%
  # distinct() %>%
  inner_join(otg_raw_all$cu)
  # inner_join(otg$cu)

