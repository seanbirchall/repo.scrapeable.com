\name{somer_list}
\alias{somer_list}
\title{List available datasets on Somerville Open Data}
\usage{somer_list(limit = 100, category = NULL)}
\arguments{
\item{limit}{Number of datasets to return (default 100)}
\item{category}{Optional category filter}
}
\value{tibble with id, name, description, category, type, updated_at, view_count}
\description{Returns a catalog of datasets from data.somervillema.gov.}
