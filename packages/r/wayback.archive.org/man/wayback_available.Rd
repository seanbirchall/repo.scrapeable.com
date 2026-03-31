\name{wayback_available}
\alias{wayback_available}
\title{Check if a URL has snapshots in the Wayback Machine}
\description{
Check if a URL has snapshots in the Wayback Machine
}
\usage{
wayback_available(url, timestamp = NULL)
}
\arguments{
\item{url}{URL to check (e.g. "https://example.com")}

\item{timestamp}{Optional timestamp to check closest to (format: YYYYMMDD)}
}
\value{
tibble: url, available, snapshot_url, timestamp, status
}

