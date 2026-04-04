\name{hud_fmr}
\alias{hud_fmr}
\title{Get HUD Fair Market Rents data}
\usage{
hud_fmr(year = 2026, state = NULL)
}
\arguments{
\item{year}{Fiscal year (e.g. 2026). Default: 2026.}
\item{state}{Optional 2-letter state abbreviation to filter results}
}
\value{
Tibble with FMR data by county/area including 0-4 bedroom rent values
}
\description{
Downloads the official FMR spreadsheet for a given fiscal year from HUD User.
}
