\name{rdap_domain}
\alias{rdap_domain}
\title{RDAP domain lookup}
\description{
Queries RDAP for domain registration information.   expiration, last_changed, nameservers
}
\usage{
rdap_domain(domain)
}
\arguments{
\item{domain}{Domain name (e.g. "example.com", "google.com")}
}
\value{
tibble: name, handle, status, registrar, registration,
}

