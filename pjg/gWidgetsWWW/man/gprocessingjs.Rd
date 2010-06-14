\name{gprocessingjs}
\alias{gprocessingjs}
\title{Interface to Processing.js code to create graphics with javascript}
\description{

	This function provides the ability to write to canvas on the
	browser using somewhat familar graphics commands in R. The
	processing.js (\url{http://ejohn.org/blog/processingjs})
	javascript library is used to provide an interface to the
	canvas web object.
	\cr

	The package also provides \code{gcanvas}, for the \pkg{canvas}
	device, which uses the same technology to display graphics,
	but using R's plotting commands.
	

}
\usage{
gprocessingjs(width=400, height=400, pointsize= 12, container = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{width}{width of canvas in pixels}
  \item{height}{height of canvas in pixels}
  \item{pointsize}{size of font in pixels}
  \item{container}{parent container for processing object}
  \item{\dots}{not used}
}
\details{



  This package is an interface to the processing.js javascript libraries  \url{http://ejohn.org/apps/processing.js/processing.js} and 
  \url{http://ejohn.org/apps/processing.js/examples/init.js}. These provide the processing API for javascript. (Processing is an API for JAVA-based graphics.

  The \code{gcanvas} widget is more straightforward to use, but this
  widget is still included, as it can be used to create relatively
  interactive graphics with the local server version of
  \pkg{gWidgetsWWW}. For this use, no installation is required.

  This does not work like a typical R device.  The constructor returns
  a \code{proto} object, for which several methods are defined,
  including many familiar graphics primitives. Methods for proto
  objects are called through the object-oriented notation
  \code{obj\$methodname(arguments)}.
  
  The \code{proto} object has properties \code{..margin} and
  \code{..background} that may be changed directly to adjust the defaults.
  
  The main methods have only an abbreviated subset of the graphical
  parameters available.

  The following familiar methods are implemented:

  \describe{
    \item{p\$plot.new()}{
      Simply sets the background color to the value in the
      \code{..background} property
    }

    \item{p\$plot.window(xlim, ylim, ...)}{
      Set the user limits for the
      graphic. The underlying canvas values are in pixels, this allows
      conversion into x-y coordinates.
    }
    
    \item{p\$title(main=NULL, sub=NULL, xlab=NULL, ylab=NULL, cex=1}{
      Set the titles etc. for the graphic. The ylabel is not rotated.
    }
    
    \item{p\$axis(side)}{
      Draw axis on side given by \code{side}. Uses \code{pretty} to label axis.
    }
    
    \item{p\$box()}{
      Draw a box around graphic area
    }
    
    \item{p\$lines(x, y=NULL, col=NA, lwd=1, ...}{
      Draw lines between values in x and values in y
    }
    
    \item{p\$points(x,y=NULL, cex=1, col=NA, ...}{
      Draw points centered at x,y. The interior color is adjusted by \code{col}
    }

    \item{p\$polygon(x, y=NULL, col=NA}{
      Draw polygon using opints in \code{x} and \code{y}. The \code{col}
      sets the interior fill.
    }
    
    \item{p\$rect(xleft, ybottom, xright, ytop, col=NA, ...)}{
      Draw rectangle(s) filling with the color if given.
    }

    \item{p\$abline(a=NULL, b=NULL, v=NULL, h=NULL, coef=NULL, ...)}{
      draws line over the canvas
    }
    
    \item{p\$text(x, y = NULL, labels = seq\_along(x),cex = 1, col = NULL, pos = NULL, ...)}{
      Places text label at x,y position. The value of pos adjusts
      the text: 1 for below, 2 to left, 3 above, 4 to right, 0 center.
    }
  }
  
  There are also several methods from the processing API that are
  implemented. The processing API allows for interactive graphics. The
  example shows how one can use a slider to adjust a histogram.

  This can be kind of slow, as the lag between the browser and R can
  make the interface non-responsive. This isn't so bad with the local version. If one wishes to write some
  javascript code, the methods "mouseDragged","mouseMoved", "mousePressed", "mouseReleased",
  "keyPressed","keyReleased","draw","setup" can be set to functions that
  produce javascript handlers. This way there is not a lag between the
  browser and the R process.
  
}
\value{
  Returns a \pkg{proto} object with class gProcessing.
}
\references{\url{http://ejohn.org/blog/processingjs}}
\author{John Verzani }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\examples{
\dontrun{
   ## make a histogram with breaks controlled by a slider
   w <- gwindow("processing example")
   g <- ggroup(horizontal=FALSE, cont=w)
   x <- faithful$eruptions
   p <- gprocessingjs(width=500, height = 400, container  = g)
   p$textFont("arial",14)
   p$Hist <- function(.,x, breaks = "Sturges", col = "goldenrod") {
     out <- hist(x, breaks = breaks, plot=FALSE)
     p$plot.new()
     p$plot.window(xlim = range(out$breaks), ylim = c(0, max(out$counts)))
     p$axis(1)
     p$title(main = deparse(substitute(x)))
     nb <- length(out$breaks)
     p$rect(out$breaks[1:(nb-1)], rep(0, length=nb-1), out$breaks[2:nb],
       out$counts, col)
   }
   p$Hist(faithful$eruptions)
   gslider(from = 1, to = length(x), by = 1, cont = g, handler =
   function(h,...) {
     p$Hist(faithful$eruptions, breaks =as.numeric(svalue(h$obj)))
   })


 ## Make regression line that follows mouse clicks
 x <- c(); y <- c()
 glabel("Click in graph to set points", container=g)
 p1 <- gprocessingjs(width=500, height = 400, container  = g)
 p1$..background <- 200
 p1$plot.new()
 p1$plot.window(xlim=c(0,10), ylim=c(0,10))
 addHandlerMouseclick(p1, handler = function(h,...) {
   xy <- p1$pixelsToXY(h$xy)
   assign("x",c(x, xy[1]), envir=.GlobalEnv)
   assign("y",c(y, xy[2]), envir=.GlobalEnv)
   p1$plot.new() # paints background
   p1$points(x, y, cex=2, col="red")
   if(length(x) != 1) {
     res <- lm(y ~ x)
     p$abline(res,col="blue")
   }
 })

   visible(w) <- TRUE

   ## This is really slow!
w <- gwindow("Follow the cursor", cont=g)
w$doLoadingText <- FALSE

f <- gframe("Regression example", horiz=FALSE, cont = w)
glabel("Move mouse through canvas and red ball will follow along", container=f)
p1 = gprocessingjs(width=500, height = 400, container  = f)
p1$..background = 200
p1$plot.new()
p1$plot.window(xlim=c(0,10), ylim=c(0,10))

theta <- seq(0, 2*pi, length=100)
inds <- 2*(0:4) + 1.5
p1$addHandlerMousemove(handler = function(h,...) {
  xy = p1$pixelsToXY(h$xy) ## need to convert to user coordinates
  p1$plot.new()
  for(i in inds) {
    for(j in inds) {
      r <- min(0.5, sqrt((i-xy[1])^2 + (j-xy[2])^2)/5)
      p1$points(i, j, cex=r*10, col="red")
    }
  }
})


gstatusbar("Powered by gWidgetsWWW", cont=w)
visible(w) <- TRUE



}
}
\keyword{interface}

