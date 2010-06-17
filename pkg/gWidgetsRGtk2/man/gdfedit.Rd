\name{gdfedit}
\alias{gdfedit}
\alias{.gdfedit}
\title{gWidgets interface for RGtk2DfEdit widget}
\description{
	An alternative widget for editing a data frame using the
	RGtk2DfEdit package
}
\usage{
gdfedit(items = NULL,
        name = paste(deparse(substitute(items)), "1", sep="."),
        container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{items}{data frame to be edited}
  \item{name}{Name of data frame to save value to}
  \item{container}{An optional container to attach widget to}
  \item{\dots}{Can be used to overide default colors.}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  
  The \code{gdf} widget is used for editing data frames, but does not
  have the most natural keyboard handling. The RGtk2DfEdit package by
  Tom Taverner provides an easier to use interface for editing data
  frame, and this wraps that widget into gWidgetsRGtk2.
 

  The \code{addHandlerColumnClicked} function can be used to add a
  handler to the event when a column header is clicked. The component of
  the first argument \code{column.no} contains the column number.

  This widget is a bit different from the others as it is not imported
  from gWidgets. As such, it won't exist until this
  \pkg{gWidgetsRGtk2} package is loaded. In practical terms, you need
  to realize a widget before this one can be realized.

}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{\code{\link{gtable}}}
\examples{
\dontrun{
w<-gwindow()
g<-ggroup(cont=w)
df<-gdfedit(iris, cont=g)

## check names
names(df)
names(df)[1]<-"new"
rownames(df)
colnames(df)

## check [
df[,]
df[1,]
df[,1]

## no [<- function
## check dim stuff
dim(df)
length(df)

## handler

addHandlerColumnClicked(df, handler<-function(h,...) {
  print(h$column.no)
})

	
}
}
\keyword{interface }
