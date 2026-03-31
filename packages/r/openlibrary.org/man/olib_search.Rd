\name{olib_search}
\alias{olib_search}
\title{Search Open Library for books}
\description{
  isbn, subject, edition_count
}
\usage{
olib_search(query, limit = 20)
}
\arguments{
\item{query}{Search query (e.g. "lord of the rings", "machine learning")}

\item{limit}{Number of results (default 20, max 100)}
}
\value{
tibble: key, title, author_name, first_publish_year, publisher,
}

