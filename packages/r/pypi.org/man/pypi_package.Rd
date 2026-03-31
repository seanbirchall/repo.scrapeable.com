\name{pypi_package}
\alias{pypi_package}
\title{Get PyPI package metadata}
\description{
Returns metadata for a Python package from PyPI, including the latest version info, author, license, and requirements.   home_page, package_url, requires_python, keywords
}
\usage{
pypi_package(name)
}
\arguments{
\item{name}{Package name (e.g. "requests", "numpy", "pandas")}
}
\value{
tibble: name, version, summary, author, author_email, license,
}

