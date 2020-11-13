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
#'     \item{tmp}{description text}
#'   }
#'
#'   \item \strong{wood} A tibble with 84 rows and 14 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{tmp}{description text}
#'   }
#'
#'   \item \strong{jam} A tibble with 12 rows and 12 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{tmp}{description text}
#'   }
#'
#'   \item \strong{undercut} A tibble with 99 rows and 14 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{tmp}{description text}
#'   }
#'
#'   \item \strong{discharge} A tibble with 5 rows and 9 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{tmp}{description text}
#'   }
#'
#'   \item \strong{discharge_measurements} A tibble with 93 rows and 11 columns
#'   \describe{
#'     \item{path_nm}{relative path to the source file}
#'     \item{tmp}{description text}
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
#' }
"init_qc"
