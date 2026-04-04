\name{perf_cx_survey}
\alias{perf_cx_survey}
\title{Get CX survey data}
\description{
Returns a tibble of Customer Experience survey results with question-level
response counts, point scales, and metric categories across federal services.
}
\usage{
perf_cx_survey(agency = NULL, year = NULL)
}
\arguments{
\item{agency}{Optional agency/organization name filter (partial match, case-insensitive).}
\item{year}{Optional fiscal year filter (integer).}
}
\value{
A tibble of CX survey data.
}
