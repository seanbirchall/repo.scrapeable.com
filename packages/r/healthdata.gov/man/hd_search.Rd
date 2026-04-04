\name{hd_search}
\alias{hd_search}
\title{Search HealthData.gov datasets}
\description{
Searches the catalog by name (case-insensitive partial match).
}
\usage{
hd_search(query)
}
\arguments{
\item{query}{Search string}
}
\value{
A tibble with columns: name, view_id, category
}
