## code to prepare `init_qc` dataset goes here

library(DASH)

# load `otg_raw` data from R package
data(otg_raw)

init_qc = qc_wrapper(survey_df = otg_raw$survey,
                     cu_df = otg_raw$cu,
                     wood_df = otg_raw$wood,
                     jam_df = otg_raw$jam,
                     undercut_df = otg_raw$undercut,
                     discharge_df = otg_raw$discharge,
                     discharge_meas_df = otg_raw$discharge_measurements,
                     redirect_output = F)

write_csv(init_qc, "data-raw/init_qc.csv")
usethis::use_data(init_qc,
                  overwrite = T)
