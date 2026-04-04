\name{nola_311}
\alias{nola_311}
\alias{nola_calls_for_service}
\alias{nola_permits}
\alias{nola_code_enforcement}
\alias{nola_use_of_force}
\alias{nola_misconduct}
\alias{nola_businesses}
\alias{nola_str_licenses}
\alias{nola_fetch_all}
\title{NOLA data access functions}
\usage{
nola_311(type = NULL, status = NULL, since = NULL, limit = 1000, offset = 0)
nola_calls_for_service(type = NULL, district = NULL, since = NULL, limit = 1000, offset = 0)
nola_permits(type = NULL, status = NULL, since = NULL, limit = 1000, offset = 0)
nola_code_enforcement(status = NULL, since = NULL, limit = 1000, offset = 0)
nola_use_of_force(level = NULL, disposition = NULL, since = NULL, limit = 1000, offset = 0)
nola_misconduct(disposition = NULL, since = NULL, limit = 1000, offset = 0)
nola_businesses(q = NULL, limit = 1000, offset = 0)
nola_str_licenses(license_type = NULL, limit = 1000, offset = 0)
nola_fetch_all(dataset_id, where = NULL, q = NULL, max_rows = 10000, page_size = 1000)
}
\description{Named data access functions for popular NOLA datasets.}
