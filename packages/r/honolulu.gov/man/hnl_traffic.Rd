\name{hnl_traffic}
\alias{hnl_traffic}
\title{Get traffic incidents}
\usage{
hnl_traffic(limit = 1000, type = NULL)
}
\arguments{
\item{limit}{Maximum rows (default 1000).}
\item{type}{Optional incident type filter (e.g. "MVC").}
}
\value{
A tibble with columns: date, time, type, address, location, area.
}
\description{
Real-time traffic incidents updated every 5 minutes.
}
