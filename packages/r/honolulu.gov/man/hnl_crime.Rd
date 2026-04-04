\name{hnl_crime}
\alias{hnl_crime}
\title{Get HPD crime incidents}
\usage{
hnl_crime(limit = 1000, type = NULL)
}
\arguments{
\item{limit}{Maximum rows (default 1000).}
\item{type}{Optional crime type filter (e.g. "ROBBERY").}
}
\value{
A tibble with columns: objectid, incidentnum, blockaddress, date, type, status, score, side.
}
\description{
Honolulu Police Department crime incidents.
}
