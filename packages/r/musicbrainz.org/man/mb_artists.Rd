\name{mb_artists}
\alias{mb_artists}
\title{Search MusicBrainz for artists}
\description{
Search MusicBrainz for artists
}
\usage{
mb_artists(query, limit = 25)
}
\arguments{
\item{query}{Search query (e.g. "radiohead", "miles davis")}

\item{limit}{Number of results (default 25, max 100)}
}
\value{
tibble: id, name, sort_name, type, country, score, disambiguation
}

