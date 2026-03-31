\name{npm_search}
\alias{npm_search}
\title{Search npm registry for packages}
\description{
  publisher, links_npm
}
\usage{
npm_search(text, size = 20)
}
\arguments{
\item{text}{Search query string}

\item{size}{Number of results to return (default 20, max 250)}
}
\value{
tibble: name, version, description, keywords, date, author,
}

