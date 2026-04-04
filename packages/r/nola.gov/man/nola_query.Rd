\name{nola_query}
\alias{nola_query}
\title{Query any NOLA dataset via SODA}
\usage{
nola_query(dataset_id, where = NULL, select = NULL,
  group = NULL, order = NULL, q = NULL, limit = 1000, offset = 0)
}
\arguments{
\item{dataset_id}{Four-by-four Socrata dataset identifier}
\item{where}{SoQL WHERE clause}
\item{select}{SoQL SELECT clause}
\item{group}{SoQL GROUP BY clause}
\item{order}{SoQL ORDER BY clause}
\item{q}{Full-text search query}
\item{limit}{Max rows to return (default 1000)}
\item{offset}{Pagination offset (default 0)}
}
\value{tibble of query results}
\description{Low-level function to query any dataset on data.nola.gov using SoQL.}
