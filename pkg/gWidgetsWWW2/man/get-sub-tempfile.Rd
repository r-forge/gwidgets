\name{get_tempfile}
\alias{get_tempfile}
\title{Get a temporary file matching the given extension, if given.}
\usage{get_tempfile(ext=".txt")
}
\description{Get a temporary file matching the given extension, if given.}
\details{This tempfile is stored in a directory that can be served as a static file through
gWidgetsWWW2.}
\arguments{\item{ext}{file extension}
}
\examples{f <- get_tempfile(ext=".svg")
get_tempfile_url(f)}

