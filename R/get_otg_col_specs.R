#' @title Get/Set Column Specifications for On-The-Ground .csv Data
#'
#' @description Get/set column specifications for on-the-ground (OTG) data
#' collected using the DASH protocol.
#'
#' @author Mike Ackerman
#'
#' @param otg_type what type of on-the-ground (OTG) data would the user like to deal with? As of
#' the initiation of the function a character vector of the file name e.g., "surveyPoint_0.csv",
#' "CU_1.csv", etc.
#'
#' @import readr
#' @export
#' @return an object of class `col_spec`

get_otg_col_specs = function(otg_type = c("surveyPoint_0.csv",
                                          "CU_1.csv",
                                          "Wood_2.csv",
                                          "Jam_3.csv",
                                          "Undercut_4.csv",
                                          "Discharge_5.csv",
                                          "DischargeMeasurements_6.csv")) {

  # Survey Info
  if(otg_type == "surveyPoint_0.csv") {
    col_types = readr::cols(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      `Stream Name` = readr::col_character(),
      `Site Name` = readr::col_character(),
      `Survey Start Date Time` = readr::col_character(),
      HiddenStart = readr::col_character(),
      HiddenEnd = readr::col_character(),
      `Survey Crew` = readr::col_character(),
      `Water Temp (C)` = readr::col_double(),
      `Conductivity (ms)` = readr::col_double(),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      EditDate = readr::col_character(),
      Editor = readr::col_character(),
      x = readr::col_double(),
      y = readr::col_double()
    )
  } # end surveyPoint_0

  # # Survey Info (old)
  # if(otg_type == "surveyPoint_0.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Survey Time` = readr::col_time(),
  #     `Site Name` = readr::col_character(),
  #     #`Survey Date` = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     `Survey Date` = readr::col_character(),
  #     `Survey Crew` = readr::col_character(),
  #     `Conductivity (ms)` = readr::col_double(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character(),
  #     x = readr::col_double(),
  #     y = readr::col_double()
  #   )
  # } # end surveyPoint_0.csv

  # Channel Unit
  if(otg_type == "CU_1.csv") {
    col_types = readr:colls(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      `Channel Segment Number` = readr::col_integer(),
      `Channel Unit Number` = readr::col_integer(),
      `Channel Unit Type` = readr::col_character(),
      `Maximum Depth (m)` = readr::col_double(),
      `Thalweg Exit Depth (m)` = readr::col_double(),
      `Channel Unit Notes` = readr::col_character(),
      TOS = readr::col_logical(),
      BOS = readr::col_logical(),
      `Overhanging (%)` = readr::col_double(),
      `Aquatic Vegetation (%)` = readr::col_double(),
      `Woody Debris (%)` = readr::col_double(),
      `Artificial (%)` = readr::col_double(),
      `Total No Cover (%)` = readr::col_double(),
      `Cover Sum (%)` = readr::col_double(),
      `Sand/Fines <2mm (%)` = readr::col_double(),
      `Gravel 2-64mm (%)` = readr::col_double(),
      `Cobble 64-256mm (%)` = readr::col_double(),
      `Boulder 256mm (%)` = readr::col_double(),
      `Ocular Sum (%)` = readr::col_double(),
      `Pebble 1 (mm)` = readr::col_double(),
      `Pebble 2 (mm)` = readr::col_double(),
      `Pebble 3 (mm)` = readr::col_double(),
      `Pebble 4 (mm)` = readr::col_double(),
      `Pebble 5 (mm)` = readr::col_double(),
      `Pebble 6 (mm)` = readr::col_double(),
      `Pebble 7 (mm)` = readr::col_double(),
      `Pebble 8 (mm)` = readr::col_double(),
      `Pebble 9 (mm)` = readr::col_double(),
      `Pebble 10 (mm)` = readr::col_double(),
      `Pebble 11 (mm)` = readr::col_double(),
      `Width 1` = readr::col_double(),
      `Width 2` = readr::col_double(),
      `Width 3` = readr::col_double(),
      `Width 4` = readr::col_double(),
      `Width 5` = readr::col_double(),
      ParentGlobalID = readr::col_character(),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      EditDate = readr::col_character(),
      Editor = readr::col_character()
    )
  } # end CU_1.csv

  # # Channel Unit Form (old)
  # if(otg_type == "CU_1.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Channel Unit Type` = readr::col_character(),
  #     `Channel Unit Number` = readr::col_integer(),
  #     `Channel Segment Number` = readr::col_integer(),
  #     `Thalweg Exit Depth (m)` = readr::col_double(),
  #     `Channel Unit Notes` = readr::col_character(),
  #     `Overhanging Cover` = readr::col_double(),
  #     `Aquatic Vegetation` = readr::col_double(),
  #     `Woody Debris Cover` = readr::col_double(),
  #     `Artificial Cover` = readr::col_double(),
  #     `Total No Cover` = readr::col_double(),
  #     `Sand/Fines 2mm` = readr::col_double(),
  #     `Gravel 2-64mm` = readr::col_double(),
  #     `Cobble 64-256mm` = readr::col_double(),
  #     `Boulder 256mm` = readr::col_double(),
  #     `Pebble 1 (mm)` = readr::col_double(),
  #     `Pebble 2 (mm)` = readr::col_double(),
  #     `Pebble 3 (mm)` = readr::col_double(),
  #     `Pebble 4 (mm)` = readr::col_double(),
  #     `Pebble 5 (mm)` = readr::col_double(),
  #     `Pebble 6 (mm)` = readr::col_double(),
  #     `Pebble 7 (mm)` = readr::col_double(),
  #     `Pebble 8 (mm)` = readr::col_double(),
  #     `Pebble 9 (mm)` = readr::col_double(),
  #     `Pebble 10 (mm)` = readr::col_double(),
  #     `Pebble 11 (mm)` = readr::col_double(),
  #     `Maximum Depth (m)` = readr::col_double(),
  #     `Width 1` = readr::col_double(),
  #     `Width 2` = readr::col_double(),
  #     `Width 3` = readr::col_double(),
  #     `Width 4` = readr::col_double(),
  #     `Width 5` = readr::col_double(),
  #     ParentGlobalID = readr::col_character(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character()
  #   )
  # } # end CU_1.csv

  # Wood
  if(otg_type == "Wood_2.csv") {
    col_types = readr::cols(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      `Length (m)` = readr::col_double(),
      `Diameter (m)` = readr::col_double(),
      `Wet?` = readr::col_character(),
      `Channel Forming?` = readr::col_character(),
      `Ballasted?` = readr::col_character(),
      ParentGlobalID = readr::col_character(),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      EditDate = readr::col_character(),
      Editor = readr::col_character()
    )
  } # end Wood_2.csv

  # # Wood Form (old)
  # if(otg_type == "Wood_2.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Large Wood Number` = readr::col_integer(),
  #     `Length (m)` = readr::col_double(),
  #     `Diameter (m)` = readr::col_double(),
  #     `Wet?` = readr::col_character(),
  #     `Channel Forming?` = readr::col_character(),
  #     `Ballasted?` = readr::col_character(),
  #     ParentGlobalID = readr::col_character(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character()
  #   )
  # } # end Wood_2.csv

  # Jam
  if(otg_type == "Jam_3.csv") {
    col_types = readr::cols(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      `Length (m)` = readr::col_double(),
      `Width (m)` = readr::col_double(),
      `Height (m)` = readr::col_double(),
      `Estimated Number of Pieces` = readr::col_integer(),
      ParentGlobalID = readr::col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = readr::col_character()
    )
  } # end Jam_3.csv

  # Undercut
  if(otg_type == "Undercut_4.csv") {
    col_types = readr::cols(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      Location = readr::col_character(),
      `Length (m)` = readr::col_double(),
      `Width 25% (m)` = readr::col_double(),
      `Width 50% (m)` = readr::col_double(),
      `Width 75% (m)` = readr::col_double(),
      ParentGlobalID = readr::col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = readr::col_character()
    )
  } # end Undercut_4.csv

  # # Undercut Form (old)
  # if(otg_type == "Undercut_4.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Undercut Number` = readr::col_integer(),
  #     Location = readr::col_character(),
  #     `Length (m)` = readr::col_double(),
  #     `Width 25% (m)` = readr::col_double(),
  #     `Width 50% (m)` = readr::col_double(),
  #     `Width 75% (m)` = readr::col_double(),
  #     ParentGlobalID = readr::col_character(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character()
  #   )
  # } # end Undercut_4.csv

  # Discharge
  if(otg_type == "Discharge_5.csv") {
    col_types = readr::cols(
      ObjectID = readr::col_integer(),
      GlobalID = readr::col_character(),
      `Tape Distance (m)` = readr::col_double(),
      `Station Depth (m)` = readr::col_double(),
      `Station Velocity (m)` = readr::col_double(),
      ParentGlobalID = readr::col_character(),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      EditDate = readr::col_character(),
      Editor = readr::col_character()
    )
  } # end Discharge_5.csv

  # # Discharge Form (old)
  # if(otg_type == "Discharge_5.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Discharge Location (BOS, TOS, CU #)` = readr::col_character(),
  #     ParentGlobalID = readr::col_character(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character()
  #   )
  # } # end Discharge_5.csv

  # # Discharge Measurements Form (old)
  # if(otg_type == "DischargeMeasurements_6.csv") {
  #   col_types = readr::cols(
  #     ObjectID = readr::col_integer(),
  #     GlobalID = readr::col_character(),
  #     `Station Width` = readr::col_double(),
  #     `Station Depth` = readr::col_double(),
  #     `Station Velocity` = readr::col_double(),
  #     ParentGlobalID = readr::col_character(),
  #     #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     CreationDate = readr::col_character(),
  #     Creator = readr::col_character(),
  #     #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
  #     EditDate = readr::col_character(),
  #     Editor = readr::col_character()
  #   )
  # } # end DischargeMeasurements_6.csv

  return(col_types)

} # end get_otg_col_specs()

