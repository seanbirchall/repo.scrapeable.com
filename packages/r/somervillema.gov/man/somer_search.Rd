\name{somer_search}
\alias{somer_search}
\title{Search Somerville datasets by keyword}
\usage{somer_search(query, limit = 50)}
\arguments{
\item{query}{Search string}
\item{limit}{Max results (default 50)}
}
\value{tibble with id, name, description, category, type, updated_at, view_count}
\description{Full-text search across dataset names and descriptions.}
