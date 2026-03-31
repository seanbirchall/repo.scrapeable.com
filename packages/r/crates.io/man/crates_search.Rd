\name{crates_search}
\alias{crates_search}
\title{Search crates.io for Rust crates}
\description{
  updated_at, homepage, repository, categories
}
\usage{
crates_search(query, per_page = 20)
}
\arguments{
\item{query}{Search query string (e.g. "serde", "async runtime")}

\item{per_page}{Number of results per page (default 20, max 100)}
}
\value{
tibble: name, description, max_version, downloads, created_at,
}

