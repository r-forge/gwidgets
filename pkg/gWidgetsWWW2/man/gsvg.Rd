\name{gsvg}
\alias{gsvg}
\title{widget to display svg files...}
\usage{gsvg(f, width=480, height=400, container, ..., ext.args)
}
\description{widget to display svg files}
\details{This widget displays an svg file, such as is made by the
\code{svg} driver or the \code{devSVGTips} driver in the
\pkg{RSVGTipsDevice}. The basic usage is like \code{gimage}, where
a file that is stored in a place the web browser can serve is used
as the file specified to the device. The convenience method
\code{get_tempfile} (and function) is used to create such a
file. Use the \code{.svg} extension. The widget size is specified
in pixels, but the svg device driver is in inches.}
\value{An ExtWidget instance}
\note{The package has similar widgets: \code{\link{gimage}} and \code{\link{gcanvas}}.}
\arguments{\item{f}{filename. Usually produced by \code{get_tempfile(ext=".svg")}.}
\item{width}{width of widget in pixels}
\item{height}{height of widget in pixels}
\item{container}{parent container}
\item{...}{passed to \code{add} method of parent container}
\item{ext.args}{Means to pass additional arguments to Ext constructor}
}
\examples{\dontrun{
w <- gwindow("hello", renderTo="replaceme")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
require(RSVGTipsDevice)

f <- get_tempfile(ext=".svg") ## use this extension
devSVGTips(f)
hist(rnorm(100))
dev.off()

i <- gsvg(f, container=g, width=480, height=480)



b <- gbutton("click", cont=g, handler=function(h,...) {
f <- get_tempfile(ext=".svg")
devSVGTips(f)
hist(rnorm(100))
dev.off()
svalue(i) <- f
})
}}

