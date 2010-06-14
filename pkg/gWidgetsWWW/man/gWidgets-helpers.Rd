\name{gWidgetsWWW-helpers}
\alias{gWidgetsWWW-helpers}
\alias{asURL}
\alias{escapeHTML}
\alias{isURL}
\alias{runHandler}
\alias{shQuoteEsc}
\alias{unescapeURL}
\alias{String}
\alias{getStaticTmpFile}
\alias{convertStaticFileToUrl}

\title{Helper functions in gWidgetsWWW}
\description{
  These functions are simple convenience functions.
}
\details{
  The function \code{escapeHTML} converts HTML, such as $<$ into 
  a corresponding entity. The reverse is done \code{unescapeURL}, so
  named as such encoding is typically done in URLs.
  \cr
  
  The function \code{asURL} simply adds the class \code{URL} to a
  character. This allows \pkg{gWidgetsWWW} to identify local URLs that
  are not prefaced with \code{http://}.
  \cr

  The function \code{String} creates an object of class
  \code{String}. This class treats a character string as a vector of
  letters. (That is, "abc" is \code{c("a","b","c")}.) The method
  \code{c} can be used to combine strings. The more natural (in some
  way, but not every way) \code{+} method can also be used to join
  strings. This is the primary convenience of this class. Otherwise, the
  \code{[} method is useful for string extraction. The \code{length}
  method is an alternative to \code{nchar}.
  \cr

  The need for temporary files that can be accessed through a URL
  gives rise to the functions \code{getStaticTmpFile}, which creates a
  temporary file, and \code{convertStaticFileToUrl} which returns the
  URL from the name of the static temporary file.

  }
\keyword{interface}% at least one, from doc/KEYWORDS
