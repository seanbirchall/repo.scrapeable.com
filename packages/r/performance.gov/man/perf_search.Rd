\name{perf_search}
\alias{perf_search}
\title{Search across all performance.gov CX datasets}
\description{
Performs a keyword search across providers, CX survey data, and
collection details. Returns matching rows from all datasets.
}
\usage{
perf_search(query)
}
\arguments{
\item{query}{Character string to search for (case-insensitive).}
}
\value{
A tibble with search results from matching datasets.
}
