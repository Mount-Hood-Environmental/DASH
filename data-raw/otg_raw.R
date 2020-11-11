## code to prepare `otg_raw` dataset goes here

library(DASH)

# path to external data loaded with DASH package
path = ("inst/extdata/1_formatted_csvs/")

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

usethis::use_data(otg_raw,
                  overwrite = T)
