\name{gtext}
\alias{gtext}
\title{A text area widget...}
\usage{gtext(text, width, height=300, font.attr, wrap=TRUE, handler, action,
    container, ..., ext.args, resizable=FALSE)
}
\description{A text area widget}
\value{an ExtWidget instance\code{gtext}: an ExtWidget instance


}
\arguments{\item{text}{initial text}
\item{width}{width in pixels}
\item{height}{height in pixels}
\item{font.attr}{Ignored. Default font attributes}
\item{wrap}{Ignored Do we wrap the tet}
\item{handler}{handler}
\item{action}{action}
\item{container}{parent container}
\item{...}{passed to add method of parent container}
\item{ext.args}{Extra arguments to pass to Ext constructor}
\item{resizable}{Ignored. (Should area be resizable on the page)}
}
\examples{w <- gwindow("gtext example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
t <- gtext("Some text with \n new lines", cont=g)
b <- gbutton("click", cont=g, handler=function(h,...) {
galert(svalue(b), parent=w)
})
b <- gbutton("change", cont=g, handler=function(h,...) {
svalue(t) <= "some new text"
})\code{gtext}: w <- gwindow("gtext example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
t <- gtext("Some text with \n new lines", cont=g)
b <- gbutton("click", cont=g, handler=function(h,...) {
galert(svalue(b), parent=w)
})
b <- gbutton("change", cont=g, handler=function(h,...) {
svalue(t) <= "some new text"
})


}

\alias{gtext}

