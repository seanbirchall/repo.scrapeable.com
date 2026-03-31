\name{olib_book}
\alias{olib_book}
\title{Get book details by ISBN from Open Library}
\description{
  number_of_pages, isbn_13, isbn_10, subjects, cover_url
}
\usage{
olib_book(isbn)
}
\arguments{
\item{isbn}{ISBN-10 or ISBN-13 (e.g. "9780140328721", "0140328726")}
}
\value{
tibble: title, authors, publishers, publish_date,
}

