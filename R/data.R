#' Data: List of on-the-ground tibbles
#'
#' A list with 7 tibbles (survey, cu, wood, jam, undercut, discharge, discharge_measurements),
#' each containing data for an `otg_type`.
#'
#' @format
#' \enumerate{
#'   \item \strong{survey} A tibble with 2 rows and 14 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{a unique identifier within `path_nm`}
#'     \item{GlobalID}{unique identifier for survey assigned by Survey123}
#'     \item{Survey Time}{time of survey}
#'     \item{Site Name}{name of site}
#'     \item{Survey Date}{date of survey}
#'     \item{Survey Crew}{name or initials of survey crew}
#'     \item{Conductivity (ms)}{conductivity at site (microsiemens)}
#'     \item{CreationDate}{date file was created by Survey123}
#'     \item{Creator}{creator of Survey123 form}
#'     \item{EditDate}{date source file was last edited}
#'     \item{Editor}{date file was created by Survey123}
#'     \item{x}{longitude}
#'     \item{y}{latitude}
#'   }
#'
#'   \item \strong{cu} A tibble with 146 rows and 39 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Channel Unit Type}{description text}
#'     \item{Channel Unit Number}{description text}
#'     \item{Channel Segment Number}{description text}
#'     \item{Thalweg Exit Depth (m)}{description text}
#'     \item{Channel Unit Notes}{description text}
#'     \item{Overhanging Cover}{description text}
#'     \item{Aquatic Vegetation}{description text}
#'     \item{Woody Debris Cover}{description text}
#'     \item{Artificial Cover}{description text}
#'     \item{Total No Cover}{description text}
#'     \item{Sand/Fines 2mm}{description text}
#'     \item{Gravel 2-64mm}{description text}
#'     \item{Cobble 64-256mm}{description text}
#'     \item{Boulder 256mm}{description text}
#'     \item{Pebble 1 (mm)}{description text}
#'     \item{Pebble 2 (mm)}{description text}
#'     \item{Pebble 3 (mm)}{description text}
#'     \item{Pebble 4 (mm)}{description text}
#'     \item{Pebble 5 (mm)}{description text}
#'     \item{Pebble 6 (mm)}{description text}
#'     \item{Pebble 7 (mm)}{description text}
#'     \item{Pebble 8 (mm)}{description text}
#'     \item{Pebble 9 (mm)}{description text}
#'     \item{Pebble 10 (mm)}{description text}
#'     \item{Pebble 11 (mm)}{description text}
#'     \item{Maximum Depth (m)}{description text}
#'     \item{Width 1}{description text}
#'     \item{Width 2}{description text}
#'     \item{Width 3}{description text}
#'     \item{Width 4}{description text}
#'     \item{Width 5}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#'
#'   \item \strong{wood} A tibble with 84 rows and 14 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Large Wood Number}{description text}
#'     \item{Length (m)}{description text}
#'     \item{Diameter (m)}{description text}
#'     \item{Wet?}{description text}
#'     \item{Channel Forming?}{description text}
#'     \item{Ballasted?}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#'
#'   \item \strong{jam} A tibble with 12 rows and 12 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Length (m)}{description text}
#'     \item{Width (m)}{description text}
#'     \item{Height (m)}{description text}
#'     \item{Estimated Number of Pieces}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#'
#'   \item \strong{undercut} A tibble with 99 rows and 14 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Undercut Number}{description text}
#'     \item{Location}{description text}
#'     \item{Length (m)}{description text}
#'     \item{Width 25% (m)}{description text}
#'     \item{Width 50% (m)}{description text}
#'     \item{Width 75% (m)}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#'
#'   \item \strong{discharge} A tibble with 5 rows and 9 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Discharge Location (BOS, TOS, CU #)}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#'
#'   \item \strong{discharge_measurements} A tibble with 93 rows and 11 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{ObjectID}{description text}
#'     \item{GlobalID}{description text}
#'     \item{Station Width}{description text}
#'     \item{Station Depth}{description text}
#'     \item{Station Velocity}{description text}
#'     \item{ParentGlobalID}{description text}
#'     \item{CreationDate}{description text}
#'     \item{Creator}{description text}
#'     \item{EditDate}{description text}
#'     \item{Editor}{description text}
#'   }
#' }
"otg_raw"

#' Data: A tibble of initial QC results
#'
#' A tibble of initial QC results for the example dataset `otg_raw`
#'
#' @format A tibble with 44 rows and 4 variables:
#' \describe{
#'   \item{source}{the `otg_type` where the potential error was identified}
#'   \item{path_nm}{the path name to the file where the potential error was identified}
#'   \item{GlobalID}{the **GlobalID** indicating the row where the potential error was identified}
#'   \item{error_message}{a message indicating the nature of the potential error}
#'   \item{location_id}{information on the site name, segment, and channel unit number were error was identified}
#' }
"init_qc"
