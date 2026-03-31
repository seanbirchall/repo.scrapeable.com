\name{secft_search}
\alias{secft_search}
\title{Search SEC EDGAR full-text search index}
\description{
Searches the EDGAR full-text search system (EFTS) for filings matching a query string. Supports date ranges and form type filters.   Can be a single string or comma-separated list.   period_ending, file_description, adsh
}
\usage{
secft_search(query, forms = NULL, start_date = NULL, end_date = NULL, limit = 40)
}
\arguments{
\item{query}{Search query string (e.g. "artificial intelligence", "13F")}

\item{forms}{Optional form type filter (e.g. "10-K", "10-Q", "8-K").}

\item{start_date}{Optional start date for filtering (Date or "YYYY-MM-DD")}

\item{end_date}{Optional end date for filtering (Date or "YYYY-MM-DD")}

\item{limit}{Maximum number of results to return (default 40, max 100)}
}
\value{
tibble: file_date, form, display_names, file_number, cik,
}

