\name{hud_inspections}
\alias{hud_inspections}
\title{Get HUD Multifamily Physical Inspection Scores}
\usage{
hud_inspections(state = NULL, min_score = NULL)
}
\arguments{
\item{state}{Optional 2-letter state abbreviation to filter results}
\item{min_score}{Optional minimum inspection score to filter}
}
\value{
Tibble with inspection data including property details, location, and scores
}
\description{
Downloads inspection scores for HUD-assisted multifamily properties.
}
