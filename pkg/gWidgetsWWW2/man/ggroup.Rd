\name{ggroup}
\alias{ggroup}
\title{Group, or box, container...}
\usage{ggroup(horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container, ...,
    width, height, ext.args)
}
\description{Group, or box, container}
\details{A box container. Can pack in left to right or top to bottom.
The expand, fill, anchor arguments are not (yet?) implemented\code{ggroup}: A box container. Can pack in left to right or top to bottom.
The expand, fill, anchor arguments are not (yet?) implemented


}
\value{an ExtContainer object\code{ggroup}: an ExtContainer object


}
\arguments{\item{horizontal}{left or right, or top to bottom (FALSE)}
\item{spacing}{body spacing}
\item{use.scrollwindow}{ignored}
\item{container}{parent container}
\item{...}{passed to add method of parent container}
\item{width}{optional width in pixels}
\item{height}{optional height in pixels}
\item{ext.args}{extra arguments to pass to Ext constructor}
}
\examples{w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE) ## top to bottom
for(i in 1:10) gbutton(i, cont=g)
## add/delete
ctr <- 1 ### for label
addRow <- function(g) {
g1 <- ggroup(cont=g)
gbutton("x", cont=g1, handler=function(h,...) {delete(g, g1)})
glabel(paste("Click x to delete", ctr), cont=g1)
ctr <<- ctr + 1
}
g1 <- gframe("add/delete", cont=g, horizontal=FALSE)
addRow(g1)
f <- gframe("Adding and deleting", cont=g, horizontal=FALSE)
gbutton("+", cont=f, handler=function(h,...) addRow(f))\code{ggroup}: w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE) ## top to bottom
for(i in 1:10) gbutton(i, cont=g)
## add/delete
ctr <- 1 ### for label
addRow <- function(g) {
g1 <- ggroup(cont=g)
gbutton("x", cont=g1, handler=function(h,...) {delete(g, g1)})
glabel(paste("Click x to delete", ctr), cont=g1)
ctr <<- ctr + 1
}
g1 <- gframe("add/delete", cont=g, horizontal=FALSE)
addRow(g1)
f <- gframe("Adding and deleting", cont=g, horizontal=FALSE)
gbutton("+", cont=f, handler=function(h,...) addRow(f))


}

\alias{ggroup}

