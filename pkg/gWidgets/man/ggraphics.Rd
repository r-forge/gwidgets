\name{ggraphics}
\alias{ggraphics}
\alias{ggraphicsnotebook}
\title{Constructor for a toolkit specific plot device and a
		   notebook to wrap plots in}
\description{
	If a toolkit provides a graphics device, such as the
	cairoDevice package does for GTK, this constructor makes
	devices that can then be embedded in other widgets. The
	notebook interface is one such example.
}
\usage{
ggraphics(width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12, container =
NULL, ..., toolkit = guiToolkit())

ggraphicsnotebook(width=dpi*6, height=dpi*6,dpi=75, container = NULL,..., toolkit = guiToolkit()) 
}
\arguments{
  \item{width}{width in pixels of device}
  \item{height}{height in pixels of device}
  \item{dpi}{scale factor for default width and height}
  \item{ps}{pointsize}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{
    Passed to add method of container. }
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	
When multiple graphics devices are present, clicking in the window of
one will make that the current device.

The \code{visible<-} method makes the object the current device.

The \code{svalue(obj, ..., value)} method will save the visible window
to the file in \code{value}. In gWidgetsRGtk2, if the window has
another window clipping part of it, this clipping will be shown. This
"hack" is needed, as \code{dev.copy} does not currently work for the
"cairo" graphic device. (In future versions, there will be support for
pdf files within cairo.)


The \code{addhandlerclicked(obj, handler, action, ...)} method where handler has
first argument \code{h} has the additional values \code{h\$x} and
\code{h\$y} where these are values
are returned using "usr" coordinates (see \code{help("par")}). (This was
in NDC coordinates)

For \pkg{gWidgetsRGtk2} there is also rubber-band selection implemented. The \code{addHandlerChanged} method can be used to call a handler when the selection is completed. See the example for how this can do something similar to "brushing".


}
% \value{}
% \author{}
% \note{}
\examples{
\dontrun{
win <- gwindow("Graphics example")
ggraphics(ps=6, container=win)
hist(rnorm(100))

## This is for gWidgetsRGtk2 only (along with cairoDevice)
library(gWidgets)
options(guiToolkit="RGtk2")

w <- gwindow("brush example", visible=FALSE)
g <- ggroup(cont=w)
tbl <- gtable(mtcars, cont=g, filter.FUN="manual")
size(tbl) <- c(300, 500)
gg <- ggraphics(cont=g)
visible(w) <- TRUE

makePlot <- function(ind) {
  plot(mpg ~ wt, mtcars)
  if(!missing(ind) && any(ind)) 
    points(mpg ~ wt, mtcars[ind,], cex=2, pch=16, col="red")
}

ID <- addHandlerChanged(gg, handler=function(h,...) {
  x <- h$x; y <- h$y

  ind <- (mtcars$wt >= x[1]) & (mtcars$wt <= x[2]) &
         (mtcars$mpg >= y[1]) & (mtcars$mpg <= y[2])

  ## udpate graphic and data frame
  makePlot(ind) 
  if(any(ind))
    visible(tbl) <- ind
})


}
}
\keyword{interface}% at least one, from doc/KEYWORDS
