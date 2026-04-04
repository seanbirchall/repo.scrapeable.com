\name{hud_search}
\alias{hud_search}
\title{Search HUD data catalog by keyword}
\usage{
hud_search(query, max_results = 20)
}
\arguments{
\item{query}{Character string to search for in title, description, and keywords}
\item{max_results}{Maximum number of results to return (default 20)}
}
\value{
Tibble of matching datasets
}
\description{
Searches the HUD data catalog by keyword across titles, descriptions, and tags.
}
