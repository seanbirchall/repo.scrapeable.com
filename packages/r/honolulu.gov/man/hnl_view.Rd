\name{hnl_view}
\alias{hnl_view}
\title{Query any Honolulu Socrata dataset by view ID}
\usage{
hnl_view(view_id, where = NULL, select = NULL, order = NULL, q = NULL, limit = 1000, offset = 0)
}
\arguments{
\item{view_id}{The Socrata 4x4 view identifier.}
\item{where}{Optional SoQL WHERE clause.}
\item{select}{Optional SoQL SELECT clause.}
\item{order}{Optional SoQL ORDER BY clause.}
\item{q}{Optional full-text search query.}
\item{limit}{Maximum rows to return (default 1000).}
\item{offset}{Row offset for pagination.}
}
\value{
A tibble.
}
\description{
Generic SODA query for any Honolulu open data view.
}
