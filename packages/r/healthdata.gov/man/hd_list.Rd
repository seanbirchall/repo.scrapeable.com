\name{hd_list}
\alias{hd_list}
\title{List all HealthData.gov datasets}
\description{
Returns a hardcoded catalog of Socrata views available on healthdata.gov.
}
\usage{
hd_list(category = NULL)
}
\arguments{
\item{category}{Optional category filter (partial match, case-insensitive)}
}
\value{
A tibble with columns: name, view_id, category
}
