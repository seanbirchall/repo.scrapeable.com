\name{pypi_releases}
\alias{pypi_releases}
\title{Get PyPI package release history}
\description{
Returns all releases (versions) for a package with upload timestamps and file details.   packagetype, filename, url
}
\usage{
pypi_releases(name, limit = 20)
}
\arguments{
\item{name}{Package name (e.g. "requests", "numpy")}

\item{limit}{Maximum number of versions to return (default 20)}
}
\value{
tibble: version, upload_time, python_version, size,
}

