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

}

# EXAMPLES: examine differences in files
# exploring would be useful for looking at issues in survey123 forms
unf_vs_for_2019lemhi$diff_files
unf_vs_for_2019lemhi$differences$`Canyon_Survey123_2019/Undercut_4.csv`
