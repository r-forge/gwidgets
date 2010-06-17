\name{gpanedgroup}
\alias{gpanedgroup}

\title{

  A paned group holds two child components with a handle, or sash,
  between them to adjust the amount of space allocated to each

}

\description{
  A constructor for a paned group.
}
\usage{
gpanedgroup(widget1=NULL, widget2=NULL, horizontal = TRUE, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget1}{Left (top) widget. Can be added at time of
  construction, or the \code{add} method can be used to add the child
  widgets one at a time.}
  \item{widget2}{Right (bottom) widget}
  \item{horizontal}{Left/right (\code{TRUE}) or top/bottom}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  The \code{add} method can be used to one child at a time.

  The \code{svalue} method returns the sash position with a value
  between 0 and 1.
  
  The \code{svalue<-} method can be used to specify the sash position
  with a value between 0 and 1.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  w <- gwindow("gpanedgroup example")
  pg <- gpanedgroup(cont=w)
  gvarbrowser(cont = pg) ## first is left/top
  gtext(cont = pg)
  svalue(pg) <- 0.25
}
}
\keyword{interface }% at least one, from doc/KEYWORDS
