\name{npm_package}
\alias{npm_package}
\title{Get npm package metadata}
\description{
Returns metadata for a specific npm package including latest version, description, license, homepage, and dependencies.   repository, created, modified, dependencies
}
\usage{
npm_package(name)
}
\arguments{
\item{name}{Package name (e.g. "express", "react", "lodash")}
}
\value{
tibble: name, version, description, license, homepage,
}

