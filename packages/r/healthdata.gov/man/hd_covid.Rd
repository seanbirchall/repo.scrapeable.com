\name{hd_covid_hospital_state}
\alias{hd_covid_hospital_state}
\alias{hd_covid_hospital_timeseries}
\alias{hd_covid_hospital_facility}
\alias{hd_covid_testing}
\alias{hd_treatments_locator}
\alias{hd_covid_policy}
\alias{hd_covid_community_national}
\alias{hd_covid_community_county}
\alias{hd_covid_survey}
\alias{hd_hospital_reporting}
\title{COVID-19 data functions}
\description{
Functions for accessing COVID-19 datasets including hospital capacity,
testing, treatments, policy orders, community profiles, and surveys.
}
\usage{
hd_covid_hospital_state(state = NULL, limit = 1000)
hd_covid_hospital_timeseries(state = NULL, start_date = NULL,
                             end_date = NULL, limit = 5000)
hd_covid_hospital_facility(state = NULL, city = NULL,
                           hospital_name = NULL, limit = 1000)
hd_covid_testing(state = NULL, start_date = NULL,
                 end_date = NULL, limit = 5000)
hd_treatments_locator(state = NULL, zip = NULL, limit = 1000)
hd_covid_policy(state = NULL, policy_type = NULL, limit = 5000)
hd_covid_community_national(limit = 1000)
hd_covid_community_county(state = NULL, county = NULL, limit = 1000)
hd_covid_survey(wave = 28, limit = 1000)
hd_hospital_reporting(state = NULL, limit = 1000)
}
\value{
A tibble of results
}
