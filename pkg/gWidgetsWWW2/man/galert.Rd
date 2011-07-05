\name{galert}
\alias{galert}
\title{quick alert message -- not modal or obtrusive (dropped from above in extjs)...}
\usage{galert(message, title="message", delay=3, parent)
}
\description{quick alert message -- not modal or obtrusive (dropped from above in extjs)}
\value{not used\code{galert}: not used


}
\arguments{\item{message}{message to display}
\item{title}{title of message}
\item{delay}{delay in seconds}
\item{parent}{parent window, typically gwindow instance. Necessary}
}
\examples{w <- gwindow()
b <- gbutton("click me", cont=w, handler=function(h,...) {
galert("That hurt", parent=w)
})\code{galert}: w <- gwindow()
b <- gbutton("click me", cont=w, handler=function(h,...) {
galert("That hurt", parent=w)
})


}

\alias{galert}

