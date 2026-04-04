\name{hd_data_inventory}
\alias{hd_data_inventory}
\alias{hd_sorns}
\alias{hd_monkeypox_research}
\alias{hd_unaccompanied_children}
\alias{hd_tribal_tanf}
\alias{hd_tycho}
\alias{hd_expiring_contracts}
\alias{hd_categories}
\alias{hd_context}
\title{HHS governance and other data functions}
\description{
Functions for HHS Data Inventory, SORNs, monkeypox research,
unaccompanied children, tribal TANF, Project Tycho, and contracts.
}
\usage{
hd_data_inventory(organization = NULL, access_level = NULL, limit = 1000)
hd_sorns(limit = 1000)
hd_monkeypox_research(limit = 1000)
hd_unaccompanied_children(limit = 1000)
hd_tribal_tanf(limit = 1000)
hd_tycho(limit = 1000)
hd_expiring_contracts(limit = 1000)
hd_categories()
hd_context()
}
\value{
A tibble of results (hd_context returns a character string)
}
