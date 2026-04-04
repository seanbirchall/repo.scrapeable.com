\name{hd_view}
\alias{hd_view}
\alias{hd_view_all}
\title{Query any HealthData.gov SODA dataset}
\description{
Generic SODA query interface for any dataset by view_id.
}
\usage{
hd_view(view_id, where = NULL, select = NULL, order = NULL,
        q = NULL, group = NULL, limit = 1000, offset = 0)

hd_view_all(view_id, where = NULL, select = NULL, order = NULL,
            q = NULL, group = NULL, max_rows = 50000)
}
\arguments{
\item{view_id}{Socrata 4-4 view identifier}
\item{where}{SoQL WHERE clause}
\item{select}{SoQL SELECT clause}
\item{order}{SoQL ORDER BY clause}
\item{q}{Full-text search query}
\item{group}{SoQL GROUP BY clause}
\item{limit}{Max rows per page (default 1000)}
\item{offset}{Pagination offset}
\item{max_rows}{Safety cap for hd_view_all (default 50000)}
}
\value{
A tibble of results
}
