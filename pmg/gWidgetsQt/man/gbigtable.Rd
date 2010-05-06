\name{gbigtable}
\alias{gbigtable}
\title{Constructor for widget to display tabular data}
\description{
  This constructor is similar to \code{gtable}, but much faster for
  larger data sets.
}

\usage{
gbigtable(items, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())
}

\arguments{
  \item{items}{A vector, matrix or data frame to be displayed. A
  vector and matrix is coerced into a data frame.}
  \item{handler}{Passed to \code{addHandlerChanged}}
  \item{action}{Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ ignored }
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  [This widget is experimental and somewhat flaky still.]\cr

  
  This widget uses a different model for the backend that makes it much
  faster than \code{gtable}. It does not allow for some other things, so
  this widget is a bit limited in comparison.\cr

  The \code{svalue} method returns the selected value which are the
  highlighted cells. It will return \code{NULL} if there is no
  selection. If \code{drop=FALSE} then the rows of the selection are
  returned. Cell highlighting is in terms of rectangular blocks. If
  \code{index=TRUE} then the corresponding indices are returned as list
  of row and column indices.\cr

  The \code{svalue<-} method does not work.\cr

  The \code{visible} method returns the visible rows, the
  \code{visible<-} method can be used to hide rows.\cr

  The data frame methods \code{[}, \code{[<-}, \code{dim},
  \code{length}, \code{dimnames}, \code{names} are implemented.\cr

  The method \code{addHandlerChanged} can be used to add a callback to
  the change in selection.\cr

  The method \code{addHandlerClicked} can be used to add a callback to
  a mouse click. The "\code{h}" components \code{row} and \code{column}
  indicate which cell was clicked.\cr

  The method \code{addHandlerDoubleclick} can be used to add a callback to
  a mouse double click. The "\code{h}" components \code{row} and \code{column}
  indicate which cell was clicked.

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
  largeData = matrix(rnorm(10000*10), ncol=10))
  obj <-  gbigtable(largeData, container=gwindow())
  svalue(obj) 
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
