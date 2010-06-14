\name{gsvg}
\alias{gsvg}
\title{Interface to RSVGTipsDevice device}
\description{
  This package allows one to show files in a web page produced by the SVG deices in \pkg{RSVGTipsDevice} or \pkg{SVGAnnotation}. The former allows one to easily place tooltips and links to URLs, the latter allows for some very fancy javascripting to make dynamic graphics.
}
\usage{
gsvg(f, width=480, height=400, container = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{optional filename storing file with output}
  \item{width}{width of canvas in pixels}
  \item{height}{height of canvas in pixels}
  \item{container}{parent container for processing object}
  \item{\dots}{not used}
}
\details{
  
  The SVG devices are non-interactive device and requires a file to be
  written to the file system. These files should have a \code{.svg}
  extension as they are severd from a web page. As such, there are
  facilities to create a temporary file in a public webspace
  \code{getStaticTempFile}. This function uses the variable
  \code{gWidgetsWWWStaticDir} which must be configured for
  \pkg{RApache}. The function \code{convertStaticFileToUrl} gives the URL for the file on the file system.

  
  The \code{svalue<-} method is used to update the graphic.
  \cr

  Unlike the \code{gcanvas} widget, only the constructor needs the
  width and height specified. The SVG graphics "S"cale to fill the
  allotted space.
  \cr

  Adding a handler to this device is not yet supported. However, the
  \pkg{SVGAnnotation} package allows one to do very interesting things
  with svg graphic files.
  \cr
  
  The \code{gcanvas} widget is a similar function for providing graphical output within a browser.
}
\value{
  Returns a \pkg{proto} object with class gSvg.
}
\references{The RSVGTipsDevice}
\seealso{\code{RSVGTipsDevice}, \code{\link{gcanvas}}}
\author{John Verzani }
\examples{
\dontrun{
  w <- gwindow("svg test")
  g <- ggroup(cont = w)
  sg <- gsvg(cont = g)
  
  makePlot <- function() {
    require(RSVGTipsDevice, quietly=TRUE, warn=FALSE) # must require within callback
    f <- getStaticTmpFile(ext=".svg")  ## use the extension or paste later
    devSVGTips(f)
    plot(rnorm(100))
    dev.off()
    f
  }

  svalue(sg) <- makePlot()
  ## can update
  b <- gbutton("click me", cont = g, handler = function(h,...) {
    svalue(sg) <- makePlot()
  })

  visible(w) <- TRUE
  
}
}
\keyword{interface}

