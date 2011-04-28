\name{gstatusbar}
\alias{gstatusbar}
\title{Status bar for gwindow instances...}
\usage{gstatusbar(text="", container, ...)
}
\description{Status bar for gwindow instances}
\details{Status for main window. Use \code{gwindow} instance for parent
container. The \code{svalue<-} method can be used to change the
value.}
\value{an ExtWidget instance}
\arguments{\item{text}{text for label}
\item{container}{parent window}
\item{...}{ignored}
}
\examples{w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook")}

