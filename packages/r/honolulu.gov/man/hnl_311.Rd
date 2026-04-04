\name{hnl_311}
\alias{hnl_311}
\title{Get HNL 311 service requests}
\usage{
hnl_311(limit = 1000, status = NULL, request_type = NULL)
}
\arguments{
\item{limit}{Maximum rows (default 1000).}
\item{status}{Optional status filter (e.g. "Closed").}
\item{request_type}{Optional request type filter (partial match).}
}
\value{
A tibble with columns: id, date_created, request_type, street, city, state, zip_code, status, description.
}
\description{
HNL 311 service requests from data.honolulu.gov.
}
