\name{rdap_ip}
\alias{rdap_ip}
\title{RDAP IP address/network lookup}
\description{
  country, status
}
\usage{
rdap_ip(ip)
}
\arguments{
\item{ip}{IP address or CIDR block (e.g. "8.8.8.8", "192.168.1.0/24")}
}
\value{
tibble: handle, name, type, start_address, end_address,
}

