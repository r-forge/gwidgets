\name{gcombobox}
\alias{gcombobox}
\alias{gdroplist}
\alias{[<-,gCombobox-method}
\title{Widgets to allow selection from a vector of items}
\description{

	A combobox allows selection of a value from a list of items
	using a popup menu. Additionally, the widget can combine a
	text entry widget for user input outside of the pre-set list
	of items. A combobox is useful for selection from a moderate
	size of items (2-30, say). For smaller sets of items, a radio
	button group is a possibility, for larger sets of items, a
	scrollable (and perhaps searchable) table may be preferred.

}
\usage{
gcombobox(items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL, 
    action = NULL, container = NULL, ..., toolkit = guiToolkit())

gdroplist(items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL, 
    action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}


\arguments{
  \item{items}{ Vector of values to select from. This may also be a data frame, in which case the first column is the vector of values, the second (if present) indicates a stock icon to display with the item (not all toolkits), and the third (if present) will indicate a tooltip to display (not all toolkits). }
  \item{selected}{ For gradio the initial selected value (as an index)
  		   For a drop list, the first selected value. Use 0 to
		     leave blank}
  \item{editable}{A logical indicating if the user can
  add an entry to the list of available answers}
  \item{coerce.with}{Apply this function to selected value before returning}
  \item{handler}{Called when selection is changed}
  \item{action}{Passed to handler when called.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{

	The initial \code{items} can be a vector or a data frame. Not
	all tool kits do something with the extra columns in the data
	frame.

	The \code{svalue} method returns the selected value by
	name. Assume the value is a character vector. Use the
	\code{coerce.with} argument to return a value of a different
	type. If the extra argument \code{index=TRUE} is specified,
	the index of the selected value is given.


	The \code{svalue<-} method can be used to set the selected
	value. This is done my name or if the argument
	\code{index=TRUE} is given by index. The value can be a data
	frame, in which case the first column is used to match against
	the current items.

	The \code{"["} method refers to the vector defining the items.

	The \code{"[<-"} method can be used to change the vector
	defining the items. 

	The \code{"length"} method returns the number of items.
	
	For \code{gcombobox} the argument \code{editable=TRUE} adds a
	text-edit box where the user can type in a selection. By
	default this value is returned as a character by
	\code{svalue}. Use \code{coerce.with} to coerce this prior to returning.

}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{

	A combobox is one of several ways to select a value of a set of items. See also 
\code{\link{gcheckbox}}, 
\code{\link{gradio}}, 
\code{\link{gcheckboxgroup}}, and
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

  w <- gwindow("combobox example")
  gp <- ggroup(container=w)
  glabel("Favorite flavor:", cont=gp)
  cb <- gcombobox(flavors, editable=TRUE, cont=gp, handler=f)

  svalue(cb) <- "vanilla"
  svalue(cb)
  cbg[3] <- "raspberry"

}

}
\keyword{interface}% at least one, from doc/KEYWORDS
