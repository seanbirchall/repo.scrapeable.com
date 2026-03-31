\name{wayback_cdx}
\alias{wayback_cdx}
\title{Search the Wayback Machine CDX index}
\description{
Queries the CDX server for archived snapshots of a URL. Returns metadata for each capture including timestamp, status, and MIME type.   "host", "domain"   digest, length
}
\usage{
wayback_cdx(url, limit = 10, from = NULL, to = NULL, matchType = "exact")
}
\arguments{
\item{url}{URL to search for (e.g. "example.com")}

\item{limit}{Maximum number of results (default 10)}

\item{from}{Optional start date (format: YYYYMMDD or YYYY)}

\item{to}{Optional end date (format: YYYYMMDD or YYYY)}

\item{matchType}{Optional match type: "exact" (default), "prefix",}
}
\value{
tibble: urlkey, timestamp, original, mimetype, statuscode,
}

