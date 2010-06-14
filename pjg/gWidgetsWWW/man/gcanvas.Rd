\name{gcanvas}
\alias{gcanvas}
\alias{ggraphics}
\title{Interface to canvas device}
\description{
	This function allows the \pkg{canvas} package to create a javascript device.
	Although this isn't working with all browsers, it does work quite well
	with firefox.
}
\usage{
gcanvas(f, width=480, height=400, container = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{optional filename storing canvas output}
  \item{width}{width of canvas in pixels}
  \item{height}{height of canvas in pixels}
  \item{container}{parent container for processing object}
  \item{\dots}{not used}
}
\details{
  Jeffrey Horner's \pkg{canvas} package provides a javascript implementation of an R device. The use if very similar to
  a regular, non-interactive R device, such as \code{png}. The \code{gcanvas} widget provides the screen space	
  to display the graphic.
  \cr

  The \code{svalue<-} method is used to update the graphic. By default
  the background color for a canvas device is transparent, so graphs
  will layer on top of each other. To avoid this, specify a background
  color using hex notation, e.g., \code{bg="#ffffff"} for white.
  \cr
  
  Only one \code{gcanvas} widget can be used in a GUI, as underlying
  the \code{canvas} javascript is a fixed variable name. This includes
  subwindows of a GUI, so this is a poor choice for using in a window
  that can be dismissed and re-opened.
    \cr
  
  Both the \code{gcanvas} constructor and the
  \code{canvas} device have width and height arguments. These should be
  the same, but it is up to the user to ensure this.
  \cr

  This replaces the \code{gprocessingjs} widget that isn't working as of version 0.8.0
}
\value{
  Returns a \pkg{proto} object with class gCanvas.
}
\references{The canvas device package}
\author{John Verzani }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\examples{
\dontrun{
  require(canvas, quietly=TRUE, warn=FALSE) ## require quietly

  ## Make a simple graph
  f <- tempfile() ## A file to store the javascript code
  canvas(file=f, width=480, height=400)
  hist(rnorm(100))
  dev.off()  ## write javascript code to f

  w <- gwindow("Canvas test")
  g <- ggroup(cont = w, horiz=F)
  cv <- gcanvas(f,cont = g)  ## when f is given, graph an initial graph
                             ## is drawn
  ## How to update the graphic using the svalue method
  b <- gbutton("click me", cont = g, handler = function(h,...) {
    canvas(file=f, width=480, height=400, bg="#ffffff") ## specify a
                                                        ## background color
    hist(rexp(100))
    dev.off()
    svalue(cv) <- f
  })

}
}
\keyword{interface}

