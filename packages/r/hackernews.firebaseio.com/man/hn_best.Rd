\name{hn_best}
\alias{hn_best}
\title{Get best Hacker News stories}
\description{
Get best Hacker News stories
}
\usage{
hn_best(n = 30)
}
\arguments{
\item{n}{Number of stories to return (default 30, max 500)}
}
\value{
tibble: id, type, title, url, by, score, descendants, time
}

