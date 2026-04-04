\name{hud_homeless_counts}
\alias{hud_homeless_counts}
\title{Get HUD Annual Homeless Assessment Report counts by state}
\usage{
hud_homeless_counts(year = 2024)
}
\arguments{
\item{year}{Year of the count (2007-2024). Default: latest available.}
}
\value{
Tibble with homeless housing inventory counts by state
}
\description{
Downloads the Housing Inventory Count (HIC) data showing shelter beds
and housing units for homeless populations by state.
}
