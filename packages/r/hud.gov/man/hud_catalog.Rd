\name{hud_catalog}
\alias{hud_catalog}
\title{List all HUD datasets from the data catalog}
\usage{
hud_catalog(refresh = FALSE)
}
\arguments{
\item{refresh}{Logical; if TRUE, re-download catalog even if cached}
}
\value{
Tibble of all datasets with title, description, keywords, URLs
}
\description{
Downloads and parses the HUD Enterprise Data Listing (data.hud.gov/data.json)
containing 220+ datasets.
}
