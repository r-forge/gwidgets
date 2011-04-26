\name{gtree}
\alias{gtree}
\title{gtree widget...}
\usage{gtree(offspring, offspring.data, icon.FUN, chosencol=1, multiple=FALSE,
    handler, action, container, ..., width, height, ext.args)
}
\description{gtree widget}
\details{Widget to display heirarchical data. The data is described by a
function which returns the offspring for agiven node in a specific
format. This implementation can only show one column's worth of
data and an icon.}
\note{TODO: implement multiple slection, grids,}
\arguments{\item{offspring}{Function with signature (path, data). Returns a
data frame with columns: id (which create the path), hasOffspring
(logical indicating if it has children), optionally an icon (a
stock icon name) and a value, which defaults to the id. The id's
must not have a ':' as that is chosen as a delimiter.}
\item{offspring.data}{passed to offspring call so that the
offspring function can be parameterized if desired.}
\item{icon.FUN}{NULL, Ignored. Place stock icons names in third column through offspring function, or \code{NULL}}
\item{chosencol}{(if/when?) grid is implemented will work with svalue method}
\item{multiple}{for multiple selection (not implemented)}
\item{handler}{called on double click}
\item{action}{passed to handler}
\item{container}{container object}
\item{...}{passed to containers add method}
\item{width}{width in pixels}
\item{height}{height in pixels}
\item{ext.args}{extra configuration arguments passed to Ext constructor}
}
\examples{# galton watson
offspring <- function(path, ...) {
x <- rbinom(2, 1, p=1/2)
icons <- c("dismiss","ok")[2-x]
nms <- c("branch","leaf")[x+1]
ttip <- paste("This is a ", nms)
data.frame(id=letters[1:2], hasoffspring=as.logical(x), icons=icons, value=nms, qtip=ttip, stringsAsFactors=FALSE)
}
w <- gwindow("Galton Watson tree")
g <- ggroup(cont=w, horizontal=FALSE)
ghtml("A node in a Galton-Watson tree has 0 or 2 offspring.<br /> In this case each has equal chance.", cont=g)
gseparator(cont=g)
tr <- gtree(offspring=offspring, icon.FUN=TRUE, cont=g)
size(tr) <- c(300,300)
b <- gbutton("Try again", cont=g, handler=function(h,...) tr$update())
visible(w) <- TRUE}

