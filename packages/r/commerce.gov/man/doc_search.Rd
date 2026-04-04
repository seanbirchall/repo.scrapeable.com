\name{doc_search}
\alias{doc_search}
\title{Search the Commerce data catalog}
\description{
Searches across title, description, keywords, and publisher fields
of the Enterprise Data Inventory. Case-insensitive.
}
\usage{
doc_search(query, field = NULL)
}
\arguments{
\item{query}{Character string to search for}
\item{field}{Optional: restrict search to "title", "description", "keywords", or "publisher".}
}
\value{
A tibble (same schema as \code{doc_catalog}).
}
