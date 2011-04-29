\name{gslider}
\alias{gslider}
\title{slider widget...}
\usage{gslider(from=0, to=100, by=1, value=from, horizontal=TRUE, handler, action,
    container, ..., width, height, ext.args, tpl="{0}")
}
\description{slider widget}
\details{Use slider to select from a sequence of values, specified as with
\code{seq}. The sequence steps must be greater than 1}
\value{an ExtWidget object}
\note{not sure why, but the icons capping the end of the slider display are off.}
\arguments{\item{from}{starting point. Unlike other implementations for
gWidgets, this is not possibly a vector specifying a range of
values to slide over.}
\item{to}{ending point}
\item{by}{step size. Must be larger than 1 and even then will round to integer value}
\item{value}{initial value}
\item{horizontal}{orientation}
\item{handler}{called when slider moved}
\item{action}{passed to handler}
\item{container}{parent container}
\item{...}{passed to \code{add} method of container}
\item{width}{width}
\item{height}{height}
\item{ext.args}{list. Can pass in other configuration arguments to Ext widget}
\item{tpl}{Template for tooltip. Should have "\code{{0}}" but can have more formatting}
}
\examples{w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
sl <- gslider(from=0, to=100, by=1, value=50, cont=w, tpl="{0}percent")
addHandlerChanged(sl, handler=function(h,...) {
galert(paste("You selected", svalue(h$obj)), parent=w)
})}

