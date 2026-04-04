\name{hnl_businesses}
\alias{hnl_businesses}
\title{Get Hawaii business registrations}
\usage{
hnl_businesses(q = NULL, status = NULL, limit = 1000)
}
\arguments{
\item{q}{Full-text search query.}
\item{status}{Optional status filter (e.g. "Active").}
\item{limit}{Maximum rows (default 1000).}
}
\value{
A tibble of business registrations.
}
\description{
Hawaii business entity registrations from data.honolulu.gov.
}
