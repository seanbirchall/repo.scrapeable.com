\name{perf_collection_details}
\alias{perf_collection_details}
\title{Get detailed CX reporting collections}
\description{
Returns the full merged CX dataset with detailed question-level data,
service descriptions, and operational metrics.
}
\usage{
perf_collection_details(agency = NULL, year = NULL)
}
\arguments{
\item{agency}{Optional agency/organization name filter (partial match, case-insensitive).}
\item{year}{Optional collection year filter (integer).}
}
\value{
A tibble of CX reporting collection details.
}
