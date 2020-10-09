#' @title Get/Set Column Specifications for On-The-Ground .csv Data
#'
#' @description Get/set column specifications for on-the-ground (otg) data
#' collected using the DASH protocol.
#'
#' @author Mike Ackerman
#'
#' @param otg_type what type of on-the-ground (otg) data would the user like to deal with? As of the iniitation of the
#' function a character vector of the file name e.g., "surveyPoint_0.csv", "CU_1.csv", etc.
#'
#' @import readr
#' @export
#' @return an object of class "col_spec"

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
      ObjectID = readr::col_double(),
      GlobalID = readr::col_character(),
      `Survey Time` = readr::col_time(),
      `Site Name` = readr::col_character(),
      #`Survey Date` = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      `Survey Date` = readr::col_character(),
      `Survey Crew` = readr::col_character(),
      `Conductivity (ms)` = readr::col_logical(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = readr::col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = readr::col_character(),
      x = readr::col_double(),
      y = readr::col_double()
    )
  } # end surveyPoint_0.csv

  # Channel Unit Form
  if(otg_type == "CU_1.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Channel Unit Type` = col_character(),
      `Channel Unit Number` = col_integer(),
      `Channel Segment Number` = col_integer(),
      `Thalweg Exit Depth (m)` = col_double(),
      `Channel Unit Notes` = col_character(),
      `Overhanging Cover` = col_double(),
      `Aquatic Vegetation` = col_double(),
      `Woody Debris Cover` = col_double(),
      `Artificial Cover` = col_double(),
      `Total No Cover` = col_double(),
      `Sand/Fines 2mm` = col_double(),
      `Gravel 2-64mm` = col_double(),
      `Cobble 64-256mm` = col_double(),
      `Boulder 256mm` = col_double(),
      `Pebble 1 (mm)` = col_double(),
      `Pebble 2 (mm)` = col_double(),
      `Pebble 3 (mm)` = col_double(),
      `Pebble 4 (mm)` = col_double(),
      `Pebble 5 (mm)` = col_double(),
      `Pebble 6 (mm)` = col_double(),
      `Pebble 7 (mm)` = col_double(),
      `Pebble 8 (mm)` = col_double(),
      `Pebble 9 (mm)` = col_double(),
      `Pebble 10 (mm)` = col_double(),
      `Pebble 11 (mm)` = col_double(),
      `Maximum Depth (m)` = col_double(),
      `Width 1` = col_double(),
      `Width 2` = col_double(),
      `Width 3` = col_double(),
      `Width 4` = col_double(),
      `Width 5` = col_double(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end CU_1.csv

  # Wood Form
  if(otg_type == "Wood_2.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Large Wood Number` = col_integer(),
      `Length (m)` = col_double(),
      `Diameter (m)` = col_double(),
      `Wet?` = col_character(),
      `Channel Forming?` = col_character(),
      `Ballasted?` = col_character(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end Wood_2.csv

  # Jam Form
  if(otg_type == "Jam_3.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Length (m)` = col_double(),
      `Width (m)` = col_double(),
      `Height (m)` = col_double(),
      `Estimated Number of Pieces` = col_integer(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end Jam_3.csv

  # Undercut Form
  if(otg_type == "Undercut_4.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Undercut Number` = col_integer(),
      Location = col_character(),
      `Length (m)` = col_double(),
      `Width 25% (m)` = col_double(),
      `Width 50% (m)` = col_double(),
      `Width 75% (m)` = col_double(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end Undercut_4.csv

  # Discharge Form
  if(otg_type == "Discharge_5.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Discharge Location (BOS, TOS, CU #)` = col_character(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end Discharge_5.csv

  # Discharge Measurements Form
  if(otg_type == "DischargeMeasurements_6.csv") {
    col_types = readr::cols(
      ObjectID = col_integer(),
      GlobalID = col_character(),
      `Station Width` = col_double(),
      `Station Depth` = col_double(),
      `Station Velocity` = col_double(),
      ParentGlobalID = col_character(),
      #CreationDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      CreationDate = readr::col_character(),
      Creator = col_character(),
      #EditDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
      EditDate = readr::col_character(),
      Editor = col_character()
    )
  } # end DischargeMeasurements_6.csv

  return(col_types)
} # end get_otg_col_specs()

