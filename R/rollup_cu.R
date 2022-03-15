#' @title Rollup Channel Unit Data
#'
#' @description Clean channel unit data (e.g., from `otg_type =` "CU_1.csv") and join
#' survey/site information to it
#'
#' @author Mike Ackerman, Kevin See, Richie Carmichael
#'
#' @param survey_df data.frame of `otg_type =` "surveyPoint_0.csv" containing information for
#' each site/survey
#' @param cu_df data.frame of `otg_type =` "CU_1.csv" containing data for each channel unit
#'
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @export
#' @return a data.frame summarizing data for channel units

rollup_cu = function(survey_df = NULL,
                     cu_df = NULL) {

  stopifnot(!is.null(survey_df))
  stopifnot(!is.null(cu_df))

  # prep survey info to attach to CUs
  join_site_df = survey_df %>%
    dplyr::select(global_id,
                  stream_name,
                  site_name,
                  survey_start_date_time,
                  survey_crew,
                  site_lon = x,
                  site_lat = y,
                  water_temp_c,
                  conductivity_ms)

  # join site info to cus and do some cleaning
  return_df = cu_df %>%
    dplyr::select(-object_id) %>%
    dplyr::select(-(creation_date:editor)) %>%
    dplyr::left_join(join_site_df,
              by = c("parent_global_id" = "global_id")) %>%
    dplyr::mutate(channel_unit_number = str_pad(channel_unit_number, 3, pad = "0"),
                  channel_segment_number = str_pad(channel_segment_number, 2, pad = "0")) %>%
    dplyr::mutate(cu_id = paste(stringr::str_remove_all(site_name, pattern = fixed(" ")),
                                channel_segment_number,
                                channel_unit_number,
                                sep = "_")) %>%
    dplyr::select(parent_global_id,
                  global_id,
                  path_nm,
                  stream_name,
                  site_name,
                  survey_start_date_time,
                  survey_crew,
                  site_lon,
                  site_lat,
                  site_water_temp_c = water_temp_c,
                  site_conductivity_ms = conductivity_ms,
                  cu_id,
                  channel_segment_number,
                  channel_unit_number,
                  channel_unit_type,
                  tos,
                  bos,
                  maximum_depth_m,
                  thalweg_exit_depth_m,
                  channel_unit_notes,
                  everything()) %>%
    dplyr::relocate(channel_unit_notes, .after = last_col())

  return(return_df)

} # end rollup_cu()
