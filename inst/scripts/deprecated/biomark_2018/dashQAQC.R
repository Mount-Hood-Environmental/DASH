#' @title DASH Data Quality Control
#'
#' @description Read DASH data and perform a quick data quality control and validation
#'
#' @author Mike Ackerman
#'
#' @param folder_name character string of the folder name where Survey123 data has been saved
#'
#' @import dplyr
#' @return NULL
#' @export

dashQAQC = function(folder_name = NULL) {
  surv_mets = read_csv(paste(folder_name, 'surveyPoint_0.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(survey_time, survey_date, survey_crew) %>%
    is.na()

  for(s in 1:length(surv_mets)) {
    if(surv_mets[s] == T)  print(paste("The ", colnames(surv_mets) [1], "column of the surveyPoint_0.csv data in", folder_name, 
    "contains a <blank> or NA"))
  }
  
  cu_mets = read_csv(paste(folder_name, 'CU_1.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, channel_unit_type, channel_unit_number, channel_segment_number, thalweg_exit_depth_m, total_no_cover, 
           maximum_depth_m) %>%
    mutate(thalweg_exit_depth_m = case_when(channel_unit_type == 'OCA' ~ 0,
                                            channel_unit_type != 'OCA' ~ thalweg_exit_depth_m))
          
  trues = which(is.na(cu_mets), arr.ind = TRUE)
  if(nrow(trues) > 0) {
    for(t in 1:nrow(trues)) {
      print(paste("The global_id", cu_mets[trues[t,1],1], "and the column", names(cu_mets)[trues[t,2]], "of the CU_1.csv data within",
                  folder_name,"contains a <blank> or NA value"))
    }
  }

  ocular = read_csv(paste(folder_name, 'CU_1.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, sand_fines_2mm:boulder_256mm) %>%
    mutate_at(c(2:5), funs(replace(., is.na(.), 0))) %>%
    mutate(ocular_sum = rowSums(.[2:5])) %>%
    select(global_id, ocular_sum)
    
  not100 = which(ocular[,2] != 0 & ocular[,2] != 100, arr.ind = TRUE)
  if(nrow(not100) > 0) {
    for(n in 1:nrow(not100)) {
      print(paste("The global_id", ocular[not100[n,1],1], "sum of ocular estimates within the", folder_name ,"directory contains a value not equal to 100"))
    }
  }
  
  wood_mets =  read_csv(paste(folder_name, 'Wood_2.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, large_wood_number:ballasted) %>%
    is.na()

  wood_trues = which(wood_mets == T, arr.ind = TRUE)
  if(nrow(wood_trues) > 0) {
    for(w in 1:nrow(wood_trues)) {
      print(paste("Row", wood_trues[w,1], "and the column", colnames(wood_mets)[wood_trues[w,2]], "within", folder_name,
      "contains a <blank> or NA value"))
    }
  }
  
  jam_mets = read_csv(paste(folder_name, 'Jam_3.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, length_m, width_m, height_m, estimated_number_of_pieces) %>%
    is.na()
  
  jam_trues = which(jam_mets == T, arr.ind = TRUE)
  if(nrow(jam_trues) > 0) {
    for(j in 1:nrow(jam_trues)) {
      print(paste("Row", jam_trues[j,1], "and the column", colnames(jam_mets)[jam_trues[j,2]], "within", folder_name,
                  "contains a <blank> or NA value"))
    }
  }
  
  und_mets = read_csv(paste(folder_name, 'Undercut_4.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, undercut_number, location, length_m, width_25_percent_m, width_50_percent_m, width_75_percent_m) %>%
    is.na()
  
  und_trues = which(und_mets == T, arr.ind = TRUE)
  if(nrow(und_trues) > 0) {
    for(u in 1:nrow(und_trues)) {
      print(paste("Row", und_trues[u,1], "and the column", colnames(und_mets)[und_trues[u,1]], "within", folder_name,
                  "contains a <blank> or NA value"))
    }
  }

  dis_mets = read_csv(paste(folder_name, 'Discharge_5.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, discharge_location_bos_tos_cu_number) %>%
    is.na()
  
  dis_trues = which(dis_mets == T, arr.ind = TRUE)
  if(nrow(dis_trues) > 0) {
    for(d in 1:nrow(dis_trues)) {
      print(paste("Row", dis_trues[d,1], "and the column", colnames(dis_mets)[dis_trues[d,1]], "within", folder_name,
                  "contains a <blank> or NA value"))
    }
  }
  
  disM_mets = read_csv(paste(folder_name, 'DischargeMeasurements_6.csv', sep = '/'), col_types = cols()) %>%
    clean_names() %>%
    select(global_id, station_width, station_depth, station_velocity) %>%
    is.na()
 
  disM_trues = which(disM_mets == T, arr.ind = TRUE) 
  if(nrow(disM_trues) > 0) {
    for(i in 1:nrow(dis_trues)) {
      print(paste("Row", disM_trues[i,1], "and the column", colnames(disM_mets)[disM_trues[i,1]], "within", folder_name,
                  "contains a <blank> or NA value"))
    }
  }

} # end dashQAQC function
