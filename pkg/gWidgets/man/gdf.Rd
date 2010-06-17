\name{gdf}
\alias{gdf}
\alias{gdfnotebook}
\title{Constructor of widget to edit a data frame}
\description{
	A widget used to edit data frames
}
\usage{
gdf(items = NULL, name = deparse(substitute(items)), do.subset = FALSE,
container = NULL, ...,toolkit = guiToolkit())

gdfnotebook(items = NULL, container = NULL, ..., toolkit=guiToolkit()) 
}
\arguments{
  \item{items}{data frame to be edited}
  \item{name}{Name of data frame}
  \item{do.subset}{A logical. If \code{TRUE} a means to filter the
  output using logical values is given.}
  \item{container}{An optional container to attach widget to}
  \item{\dots}{Can be used to overide default colors.}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	This widget is similar to that provided by \code{\link{gtable}}
	only this is intended for editing of data frames. The
	\code{gdfnotebook} widget uses a notebook to hold several data
	frames at once.


	In gWidgetsRGtk2, the table shown can be edited. Double click in
	a cell to allow this. When editing, the value is saved when the
	cell is left. This is done using the up or down arrow keys, the
	Enter key, the Tab key or the mouse. The down arrow key moves
	the cursor down a row, extending the size of the data frame if
	necessary. The Enter key moves the cursor down, but does not
	extend the data frame if at the bottom. The Tab key moves to the
	right. If at the end, a dialog to add a new variable pops up.

	Right clicking in a cell that is not currently being edited pops
	up a menu to edit the colum names, sort or apply a function to a
	row.


	If \code{do.subset=TRUE} then one can filter using subsetting  by
	a single variable. The variable may be selected by a droplist,
	the logical expression can be entered or one selected from a
	droplist.

	If more complicated filtering is desired, the
	\code{visible<-} method may be used, its value should a logical
	vector of length given by the number of rows of the data frame.
	
	
	The \code{svalue} method returns the selected value.

	The \code{svalue<-} method is used to select rows without a
	moust, the value is a set of row numbers.


	The \code{"["} method is used for data-frame like extraction
	from the object.

	The \code{"[<-"} method can be used for data-frame like
	assignment, with limitations.

	

	The \code{dim}, \code{dimnames}, \code{dimnames<-},
	\code{length}, \code{names} , and \code{names<-} methods should
	work on the object as they do for data frames.

	The \code{addhandlerchanged} handler responds to changes in
	the values of the data frame.
}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{\code{\link{gtable}}}
\examples{
\dontrun{
	obj <- gdf(mtcars, container=gwindow("mtcars"), do.subset=TRUE)
	obj[1,1]
	obj[1,]
	obj[,1]
        obj[1,1] <- 21
        obj[,] <- head(mtcars) ## replace df
}
}
\keyword{interface }
