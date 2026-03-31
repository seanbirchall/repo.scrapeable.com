\name{mb_recordings}
\alias{mb_recordings}
\title{Search MusicBrainz for recordings (songs/tracks)}
\description{
Search MusicBrainz for recordings (songs/tracks)
}
\usage{
mb_recordings(query, limit = 25)
}
\arguments{
\item{query}{Search query (e.g. "bohemian rhapsody", "so what")}

\item{limit}{Number of results (default 25, max 100)}
}
\value{
tibble: id, title, length_ms, score, artist, releases
}

