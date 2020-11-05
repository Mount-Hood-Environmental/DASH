#' @title Rollup Channel Unit Data
#'
#' @description Clean channel unit data (e.g., from `otg_type =` "CU_1.csv") and join
#' survey/site information to it
#'
#' @author Mike Ackerman, Kevin See, Richie Carmichael
#'
#' @param cu_df data.frame of `otg_type =` "CU_1.csv" containing data for each channel unit
#' @param survey_df data.frame of `otg_type =` "surveyPoint_0.csv" containing information for
#' each site/survey
#'
#' @import dplyr
#' @export
#' @return a data.frame summarizing data for channel units

rollup_cu = function(cu_df = NULL,
                     survey_df = NULL) {

  stopifnot(!is.null(cu_df))
  stopifnot(!is.null(survey_df))

  # prep survey info to attach to CUs
  join_site_df = survey_df %>%
    #mutate(site_name = gsub("_.*", "", site_name)) %>% # for 2019 surveys with years and spaces on survey names
    #mutate(site_name = gsub(" ", "", site_name)) %>%
    dplyr::select(global_id,
                  site_name,
                  survey_date,
                  survey_time,
                  survey_crew,
                  conductivity_ms,
                  site_lon = x,
                  site_lat = y)

  # join site info to cus and do some cleaning
  return_df = cu_df %>%
    dplyr::select(-object_id) %>%
    dplyr::select(-(creation_date:editor)) %>%
    dplyr::left_join(join_site_df,
              by = c("parent_global_id" = "global_id")) %>%
    #select(-parent_global_id) %>%
    dplyr::mutate(channel_unit_number = str_pad(channel_unit_number, 3, pad = "0"),
                  channel_segment_number = str_pad(channel_segment_number, 2, pad = "0")) %>%
    dplyr::mutate(cu_id = paste(site_name,
                                channel_segment_number,
                                channel_unit_number,
                                sep = "_")) %>%
    dplyr::select(parent_global_id,
                  global_id,
                  path_nm,
                  survey_date,
                  survey_time,
                  cu_id,
                  site_name,
                  channel_unit_number,
                  channel_segment_number,
                  channel_unit_type,
                  everything())

  return(return_df)

} # end rollup_cu()
