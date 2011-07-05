\name{gfile}
\alias{gfile}
\title{File selection function...}
\usage{gfile(text="Choose a file", type=c("open"), filter, handler, action,
    container, ..., width, height, ext.args)
}
\description{File selection function}
\value{an ExtWidget\code{gfile}: an ExtWidget


}
\note{the \code{svalue} method returns the temporary filename of the uploaded file, or a value of \code{NA}.the \code{svalue} method returns the temporary filename of the uploaded file, or a value of \code{NA}.}
\arguments{\item{text}{Instructional text.}
\item{type}{only "open" implemented}
\item{filter}{ignored}
\item{handler}{Ignored. called when file is selected}
\item{action}{Ignored. passed to handler}
\item{container}{parent container}
\item{...}{passed to add method of parent container}
\item{width}{width in pixels}
\item{height}{ignored}
\item{ext.args}{extra args to pass to constructor}
}
\examples{w <- gwindow()
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
f <- gfile("Choose a file for upload", cont=w, handler=function(h,...) {
galert(paste("You uploaded", svalue(h$obj)), parent=w)
})\code{gfile}: w <- gwindow()
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
f <- gfile("Choose a file for upload", cont=w, handler=function(h,...) {
galert(paste("You uploaded", svalue(h$obj)), parent=w)
})


}

\alias{gfile}

