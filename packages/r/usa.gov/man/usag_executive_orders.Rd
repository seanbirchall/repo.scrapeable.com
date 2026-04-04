\name{usag_executive_orders}
\alias{usag_executive_orders}
\title{List executive orders from the Federal Register}
\usage{
usag_executive_orders(president = NULL, per_page = 100, page = 1, year = NULL)
}
\arguments{
\item{president}{President slug: "donald-trump", "joe-biden", etc. NULL for all.}
\item{per_page}{Results per page (max 1000). Default 100.}
\item{page}{Page number. Default 1.}
\item{year}{Optional signing year filter.}
}
\value{
tibble with columns: eo_number, title, signing_date, publication_date,
citation, document_number, president, html_url, pdf_url
}
\description{
Fetches executive orders from the Federal Register API with optional
president and year filters. Supports pagination.
}
