\name{hnl_permits}
\alias{hnl_permits}
\title{Get building permits}
\usage{
hnl_permits(limit = 1000, status = NULL)
}
\arguments{
\item{limit}{Maximum rows (default 1000).}
\item{status}{Optional status filter.}
}
\value{
A tibble with key permit columns.
}
\description{
Building permits from January 2005 through June 2025.
}
