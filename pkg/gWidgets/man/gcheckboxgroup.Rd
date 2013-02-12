\name{gcheckboxgroup}
\alias{gcheckboxgroup}
\title{Widget to allow multiple selection from a vector of items}
\description{
   Widgets to select one (or several) from a given vector of
   items. These are a radio group where all values are shown at once,
   but only one may be selected; a checkbox group where more than one
   may be selected; and a
   combo box (or droplist) where initially only a single value is shown, and the others
   are a mouse click away,
}
\usage{
gcheckboxgroup(items, checked = FALSE,  horizontal = FALSE, use.table=FALSE,
    handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}


\arguments{
  \item{items}{ Vector of values to select from }
  \item{checked}{A logical vector indicating  initial values.}
  \item{horizontal}{A logical specifying the  layout for gradio and gcheckboxgroup}
  \item{handler}{Called when selection is changed}
  \item{use.table}{If \code{TRUE} a table with checkboxes will be used
  instead (toolkit depending) so that one can scroll through the values}
  \item{action}{Passed to handler when called.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{svalue} method returns the selected values by
	name. If the extra argument \code{index=TRUE} is specified,
	the indices of the selected values is given. 


	The \code{svalue<-} method can be used to set the selected
	value. This widget is a cross between a checkbox and a radio
	button group. As such, there are different ways to specify the
	state.  As with a checkbox, the argument can be a logical
	vector indicating which checkboxes are to be checked
	(recycling is done). As with a radio button group, the value
	can be a character vector indicating by label which checkboxes
	are to be checked; or if the \code{index=TRUE} argument is
	given, a vector of indices for those checkboxes to be checked.

	The \code{"["} method refers to the vector defining the items.

	The \code{"[<-"} method can be used to change the vector
	defining the items. The length should be the same as the
	original, although in some toolkits this isn't necessary.

	The \code{"length"} method returns the number of items.
	

}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{


	A checkboxgroup is one of several ways to select a value of a
	set of items. See also

\code{\link{gcheckbox}}, 
\code{\link{gradio}}, 
\code{\link{gcombobox}}, and
\code{\link{gtable}}.

Methods for gComponent objects are detailed in \code{\link{gWidgets-methods}}.

Event Handlers are detailed in \code{\link{gWidgets-handlers}}.


}
\examples{
\dontrun{
  flavors <- c("vanilla", "chocolate", "strawberry")

  f <- function(h,...) print(
      paste("Yum",
      paste(svalue(h$obj),collapse=" and "),
      sep = " "))

  w <- gwindow("checkbox example")
  gp <- ggroup(container=w)
  glabel("Favorite flavors:",container=gp)
  cbg <- gcheckboxgroup(flavors, container=gp, handler=f)

  svalue(cbg) <- c(TRUE, FALSE, TRUE)
  svalue(cbg)
  svalue(cbg) <- "vanilla"
  svalue(cbg, index=TRUE) <- 1:2
  cbg[3] <- "raspberry"

  ## use a table to display (toolkit specific) so that scrollars can be used
  cbg <- gcheckboxgroup(letters, container=gwindow(), use.table=TRUE)

}

}
\keyword{interface}% at least one, from doc/KEYWORDS
