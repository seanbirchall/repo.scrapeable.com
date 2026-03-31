\name{crates_crate}
\alias{crates_crate}
\title{Get crate details from crates.io}
\description{
  updated_at, homepage, repository, license, max_stable_version
}
\usage{
crates_crate(name)
}
\arguments{
\item{name}{Crate name (e.g. "serde", "tokio", "rand")}
}
\value{
tibble: name, description, max_version, downloads, created_at,
}

