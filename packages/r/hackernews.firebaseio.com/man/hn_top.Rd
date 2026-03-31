\name{hn_top}
\alias{hn_top}
\title{Get top Hacker News stories}
\description{
Fetches the current top stories from Hacker News. Returns full item details (title, URL, score, etc.) for the top N stories.
}
\usage{
hn_top(n = 30)
}
\arguments{
\item{n}{Number of stories to return (default 30, max 500)}
}
\value{
tibble: id, type, title, url, by, score, descendants, time
}

