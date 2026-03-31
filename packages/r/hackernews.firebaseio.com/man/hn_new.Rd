\name{hn_new}
\alias{hn_new}
\title{Get newest Hacker News stories}
\description{
Get newest Hacker News stories
}
\usage{
hn_new(n = 30)
}
\arguments{
\item{n}{Number of stories to return (default 30, max 500)}
}
\value{
tibble: id, type, title, url, by, score, descendants, time
}

