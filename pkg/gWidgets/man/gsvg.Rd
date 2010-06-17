\name{gsvg}
\alias{gsvg}
\title{Constructor for widget to show SVG files}
\description{
  Some toolkit packages provide a widget to display an SVG file. This
  widget allows this to be embedded within a gWidgets window.
}
\usage{
gsvg( filename="", width=480, height=480,
                 handler=NULL, action=NULL,
                 container = NULL, ... ,
                 toolkit=guiToolkit())
}
\arguments{
  \item{filename}{SVG file}
  \item{width}{width in pixels of widget}
  \item{height}{height in pixels of widget}
  \item{handler}{Called on click event}
  \item{action}{Used to parameterize callback specified by handler}
  \item{container}{Container to attach widget to}
  \item{\dots}{
    Passed to add method of container. }
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	
The \code{svalue} method returns the current filename.

The \code{svalue<-} method can be used to set a new file to display.

The \code{addhandlerclicked(obj, handler, action, ...)} method where handler has
first argument \code{h} has the additional values \code{h\$x} and
\code{h\$y} where these are pixel values for where the mouse click occurred.
}
% \value{}
% \author{}
\note{This is implemented in Qt only (and gWidgetsWWW).}
\examples{
\dontrun{
f = tempfile()
svg(f)
hist(rnorm(100))
dev.off()
win <- gwindow("Graphics example")
gsvg(f, container=win)
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
