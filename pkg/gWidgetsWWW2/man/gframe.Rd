\name{gframe}
\alias{gframe}
\title{gframe is a title-decorated ggroup box container...}
\usage{gframe(text="", pos=0, horizontal=TRUE, spacing=5, container, ..., width,
    height, ext.args)
}
\description{gframe is a title-decorated ggroup box container}
\details{Use \code{svalue<-} to adjust the title\code{gframe}: Use \code{svalue<-} to adjust the title


}
\value{an ExtContainer object\code{gframe}: an ExtContainer object


}
\seealso{\code{\link{ggroup}}\code{\link{ggroup}}}
\arguments{\item{text}{label text}
\item{pos}{position of label. Ignored?}
\item{horizontal}{logical. A hbox or vbox?}
\item{spacing}{between widget spacing}
\item{container}{parent container}
\item{...}{passed to add method of parent}
\item{width}{width in pixels}
\item{height}{height in pixels}
\item{ext.args}{passed to constructor}
}
\examples{w <- gwindow()
g <- gframe("Label", cont=w)
b <- gbutton("insider frame", cont=g)
svalue(g) <- "new label"\code{gframe}: w <- gwindow()
g <- gframe("Label", cont=w)
b <- gbutton("insider frame", cont=g)
svalue(g) <- "new label"


}

\alias{gframe}

