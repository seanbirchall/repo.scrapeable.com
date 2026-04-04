\name{usag_search}
\alias{usag_search}
\title{Search executive orders by keyword}
\usage{
usag_search(query, president = NULL, per_page = 20)
}
\arguments{
\item{query}{Search query string.}
\item{president}{Optional president slug to narrow results.}
\item{per_page}{Results per page. Default 20.}
}
\value{
tibble with executive order columns.
}
\description{
Searches executive orders by keyword via the Federal Register API.
}
