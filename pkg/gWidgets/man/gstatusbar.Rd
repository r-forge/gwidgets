\name{gstatusbar}
\alias{gstatusbar}

\title{Constructor of status bar widget}
\description{
  A status bar widget is used to send message to the user. A familiar
  instance is the bottom area of a web browser. 
  }
\usage{
gstatusbar(text = "", container = NULL, ..., toolkit = guiToolkit())
}

\arguments{
  \item{text}{Initial text of status bar}
  \item{container}{Optional container to attach widget to. Should be
  \code{gwindow} object}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
 The statusbar keeps a message stack. The \code{svalue} method pops the
 last message from the stack. The \code{svalue<-} method pushes a new
 message onto the stack.

 Statusbars should be added to the top-level gwindow instance.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  w <- gwindow("status bar example")
  tbl <- list(quit=list(icon="quit",
    handler = function(...) dispose(w)))
  tb <- gtoolbar(tbl, cont=w)
  sb <- gstatusbar("", cont=w)
  txt <- gtext("type here", cont=w)
  addHandlerChanged(txt, handler=function(h,...)
    svalue(sb) <- paste("You typed",svalue(txt),"in the  box",collapse=" "))

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
