\name{hud_income_limits}
\alias{hud_income_limits}
\title{Get HUD Section 8 Income Limits}
\usage{
hud_income_limits(year = 2025, state = NULL)
}
\arguments{
\item{year}{Fiscal year (e.g. 2025). Default: 2025.}
\item{state}{Optional 2-letter state abbreviation to filter results}
}
\value{
Tibble with income limits by county/area including median income,
50 percent (very low), 80 percent (low), and ELI limits by household size
}
\description{
Downloads official Section 8 income limit data from HUD User.
}
