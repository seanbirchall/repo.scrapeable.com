\name{somer_view}
\alias{somer_view}
\title{Query any Somerville dataset by view ID}
\usage{somer_view(view_id, where = NULL, select = NULL, order = NULL, q = NULL, limit = 1000, offset = 0)}
\arguments{
\item{view_id}{Socrata view ID (e.g. "mdb2-mgc7")}
\item{where}{SoQL WHERE clause}
\item{select}{SoQL SELECT clause}
\item{order}{SoQL ORDER BY clause}
\item{q}{Full-text search query}
\item{limit}{Rows to return (default 1000)}
\item{offset}{Pagination offset}
}
\value{tibble}
\description{Runs a SoQL query against a SODA 2.0 endpoint.}
