\name{cran_package}
\alias{cran_package}
\title{Get CRAN package metadata}
\description{
Returns metadata for a specific R package from CRAN via the crandb API.   license, depends, imports, date
}
\usage{
cran_package(name)
}
\arguments{
\item{name}{Package name (e.g. "dplyr", "ggplot2", "data.table")}
}
\value{
tibble: package, version, title, description, author, maintainer,
}

