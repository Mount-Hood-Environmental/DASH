% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rollup_cu.R
\name{rollup_cu}
\alias{rollup_cu}
\title{Rollup Channel Unit Data}
\usage{
rollup_cu(survey_df = NULL, cu_df = NULL)
}
\arguments{
\item{survey_df}{data.frame of \verb{otg_type =} "surveyPoint_0.csv" containing information for
each site/survey}

\item{cu_df}{data.frame of \verb{otg_type =} "CU_1.csv" containing data for each channel unit}
}
\value{
a data.frame summarizing data for channel units
}
\description{
Clean channel unit data (e.g., from \verb{otg_type =} "CU_1.csv") and join
survey/site information to it
}
\author{
Mike Ackerman, Kevin See, Richie Carmichael
}
