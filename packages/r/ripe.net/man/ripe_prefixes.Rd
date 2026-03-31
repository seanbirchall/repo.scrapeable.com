\name{ripe_prefixes}
\alias{ripe_prefixes}
\title{Get announced IP prefixes for an ASN}
\description{
Queries the RIPE Stat API for IP prefixes announced by an ASN.
}
\usage{
ripe_prefixes(resource)
}
\arguments{
\item{resource}{ASN number (e.g. "AS3333" or just "3333") or IP prefix}
}
\value{
tibble: prefix, timelines
}

