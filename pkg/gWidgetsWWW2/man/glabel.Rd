\name{glabel}
\alias{glabel}
\title{A label widget...}
\usage{glabel(text="", markup=FALSE, editable=FALSE, handler, action, container, ...,
    width, height, ext.args)
}
\description{A label widget}
\value{an ExtWidget object}
\arguments{\item{text}{text for label. Main property. Use \code{svalue<-} to change.}
\item{markup}{logical. Ignored, but see example for \code{ext.args} usage}
\item{editable}{logical. Ignored}
\item{handler}{ignored}
\item{action}{ingnored}
\item{container}{parent container}
\item{...}{passed to parent container's \code{add} method}
\item{width}{ignored}
\item{height}{ignored}
\item{ext.args}{additional options passed to Ext constructor.}
}
\examples{w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
glabel("A label widget", cont=w)
glabel("A red label", cont=w,  ext.args=list(style=list("color"="red")))}

