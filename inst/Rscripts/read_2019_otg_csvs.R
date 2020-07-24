# Authors: Mike Ackerman
#
# Purpose: A script to import all of the 2019 on-the-ground (OTG) DASH data from
# Survey 123. My intent is to later convert this script into a series of functions.
#
# Created: July 15, 2020
#   Last Modified:
#
# Notes:

#-----------------------------
# load necessary libraries
#-----------------------------
library(magrittr)

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
# set some arguments/parameters
#-----------------------------
path = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/QA_QCd_csvs/")

#-----------------------------
# read in one type of OTG data
#-----------------------------

# read CU data; as an example
cu_df = read_otg_csv(path,
                     otg_type = "CU_1.csv")

#-----------------------------
# loop over OTG data types using wrapper function
#-----------------------------
otg_data = read_otg_csv_wrapper(path = path,
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

#-----------------------------
# save the otg_data list of dfs
#-----------------------------
save(otg_data,
     file = paste0(nas_prefix,"/data/habitat/DASH/OTG/2019/lemhi/prepped/raw_DASH_2019_otg.rda"))

#-----------------------------
# testing compare two files
#-----------------------------

# the directories to compare
path1 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/raw/")
path2 = paste0(nas_prefix, "/data/habitat/DASH/OTG/2019/lemhi/QA_QCd_csvs/")

# files in path1
file_df1 = get_file_nms(path1) %>%
  dplyr::select(path_nm) %>%
  as.list() %>%
  .[[1]]

# files in path2
file_df2 = get_file_nms(path2) %>%
  dplyr::select(path_nm) %>%
  as.list() %>%
  .[[1]]

# add some things to test for diff files
file_df1 = c(file_df1, "test")
file_df2 = c(file_df2, "foo")

tst = identical(file_df1, file_df2)
tst

# files that exist in file_df1, but not in file_df2
file_df1[!(file_df1 %in% file_df2)]

# files that exist in file_df2, but not in file_df1
file_df2[!(file_df2 %in% file_df1)]

# merge
tmp_list = list()

for (f in file_df1) {

  file1 = paste0(path1, f)
  file2 = paste0(path2, f)
  tst = tools::md5sum(file1) == tools::md5sum(file2)

  if(tst == FALSE) {
    tmp_list[[ f ]] = names(tst)
    cat(paste("Found a difference in the", f, "files. Adding to the list.", "\n"))
  }
}

# store files with differences
diff_file_list = names(tmp_list)

# show differences between two files
tmp_list2 = list()

for (d in diff_file_list) {

  # the files to compare
  file1 = paste0(path1, d)
  file2 = paste0(path2, d)

  # use diffr to find differences
  diff_tmp = diffr::diffr(file1, file2, before = "file1", after = "file2")

  tmp_list2[[ d ]] = diff_tmp

}


