\name{hnl_list}
\alias{hnl_list}
\title{List all known Honolulu Open Data datasets}
\usage{
hnl_list(category = NULL)
}
\arguments{
\item{category}{Optional category filter (case-insensitive partial match).}
}
\value{
A tibble with columns: name, view_id, description, category.
}
\description{
Returns a hardcoded catalog of datasets available on data.honolulu.gov.
}
