\name{mb_releases}
\alias{mb_releases}
\title{Search MusicBrainz for releases (albums)}
\description{
Search MusicBrainz for releases (albums)
}
\usage{
mb_releases(query, limit = 25)
}
\arguments{
\item{query}{Search query (e.g. "ok computer", "kind of blue")}

\item{limit}{Number of results (default 25, max 100)}
}
\value{
tibble: id, title, status, date, country, score, artist
}

