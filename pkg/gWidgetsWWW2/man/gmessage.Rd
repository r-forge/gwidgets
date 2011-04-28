\name{gmessage}
\alias{gmessage}
\title{A simple message dialog.}
\usage{gmessage(message, title="message", icon, parent, handler, action, ...)
}
\description{A simple message dialog.}
\value{return value ignored}
\arguments{\item{message}{main message.}
\item{title}{Title for dialog's window}
\item{icon}{icon to decorate dialog}
\item{parent}{parent container (the main window instance)}
\item{handler}{Ignored. handler passed to dialog when confirmed}
\item{action}{Ignored. action passed to handler}
\item{...}{ignored}
}
\examples{w <- gwindow()
gbutton("click me for a message", cont=w, handler=function(h,...) {
gmessage("Hello there", parent=w)
})}

