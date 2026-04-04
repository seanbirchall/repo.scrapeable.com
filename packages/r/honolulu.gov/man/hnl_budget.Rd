\name{hnl_budget}
\alias{hnl_budget}
\title{Get budget data for a given fiscal year}
\usage{
hnl_budget(year = 2027, type = "proposed", limit = 1000, offset = 0)
}
\arguments{
\item{year}{Fiscal year (integer, 2014-2027).}
\item{type}{Budget type: "proposed" or "adopted".}
\item{limit}{Maximum rows (default 1000).}
\item{offset}{Row offset for pagination.}
}
\value{
A tibble of budget line items.
}
\description{
Budget data for the City and County of Honolulu.
}
