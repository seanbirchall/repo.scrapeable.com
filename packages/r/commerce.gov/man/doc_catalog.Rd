\name{doc_catalog}
\alias{doc_catalog}
\title{Commerce Enterprise Data Inventory}
\description{
Fetches and parses the Department of Commerce data.json catalog containing
~1964 dataset entries. Returns a tibble with one row per dataset.
}
\usage{
doc_catalog()
}
\value{
A tibble with columns: title, description, publisher, keywords,
modified, identifier, access_level, format, access_url,
distribution_urls, landing_page, bureau_code, license, temporal, spatial
}
