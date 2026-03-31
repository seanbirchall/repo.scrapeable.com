\name{hn_item}
\alias{hn_item}
\title{Get a single Hacker News item}
\description{
Returns details for a single HN item (story, comment, poll, job).
}
\usage{
hn_item(id)
}
\arguments{
\item{id}{Item ID (numeric)}
}
\value{
tibble: id, type, title, url, by, score, descendants, time
}

