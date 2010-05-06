\name{gqtobject}
\alias{gqtobject}
\title{Constructor to integrate Qt object into gWidgets}
\description{
  This allows integration between Qt objects and gWidgets. 
}

\usage{
gqtobject(qtobj, container=NULL, ...)
}

\arguments{
  \item{qtobj}{An instance of \code{RQtObject}}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ ignored }
}
\details{
  This constructor simply integrates the \code{RQtOBject} into the
  gwidgets framework so that the gwidgets methods can be inherited. For
  example, it allows a \code{QWidget} instance to be added to a GUI
  through a containers \code{add} method.

  An odditiy in how gWidgets works requires one to use the lowercase
  names for the \code{addHandlerXXX} methods (e.g.,
  \code{addhandlerclicked}, not \code{addHandlerClicked}) with these objects.
  
}
% \value{}
% \references{}
% \author{}
\seealso{See also
  \code{\link{gtable}} for a widget with icons, filtering, ...;
  \code{\link{gtree}} for displaying tree-like data; and
  \code{\link{gdf}} for tabular data meant to be edited
}

\examples{
\dontrun{
   b <- Qt$QPushButton("tedst")
   g <- ggroup(cont= gwindow("Test"))
   a <- gqtobject(b, container=g)
   addhandlerclicked(a, handler=function(h,...) print("hi")) ## not addHandlerClicked
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
