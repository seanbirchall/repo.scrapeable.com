\name{bb_status}
\alias{bb_status}
\title{Check availability of the broadbandmap.gov API}
\description{
Probes the old and new broadband map endpoints and reports their status.
}
\usage{
bb_status()
}
\value{
A tibble with columns: name, url, http_status.
}
