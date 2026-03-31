\name{cran_search}
\alias{cran_search}
\title{Search CRAN packages by keyword}
\description{
Searches through CRAN package names and fetches metadata for matches. Uses a case-insensitive grep on all CRAN package names via crandb.
}
\usage{
cran_search(query, limit = 20)
}
\arguments{
\item{query}{Search query string (e.g. "dplyr", "time series", "plot")}

\item{limit}{Maximum number of results (default 20)}
}
\value{
tibble: package, version, title, description, maintainer, score
}

