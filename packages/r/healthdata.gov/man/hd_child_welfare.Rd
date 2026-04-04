\name{hd_child_victims_age}
\alias{hd_child_victims_age}
\alias{hd_child_victims_trend}
\alias{hd_child_fatalities}
\alias{hd_children_disposition}
\alias{hd_maltreatment_types}
\alias{hd_cps_referrals}
\title{Child welfare (NCANDS) data functions}
\description{
Functions for accessing NCANDS child welfare datasets including
victims by age, fatalities, maltreatment types, and CPS referrals.
}
\usage{
hd_child_victims_age(state = NULL, limit = 1000)
hd_child_victims_trend(state = NULL, limit = 1000)
hd_child_fatalities(state = NULL, limit = 1000)
hd_children_disposition(state = NULL, limit = 1000)
hd_maltreatment_types(state = NULL, limit = 1000)
hd_cps_referrals(state = NULL, limit = 1000)
}
\value{
A tibble of results
}
