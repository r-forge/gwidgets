\name{gradio}
\alias{gradio}
\title{Radio button group widget}
\description{

	A radio group allows the user to select one value from a set
	of items. The items may be displayed horizontally or
	vertically.

}
\usage{
gradio(items, selected = 1, horizontal = FALSE, handler
= NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())
}


\arguments{
  \item{items}{ Vector of values to select from }
  \item{selected}{ For gradio the initial selected value (as an index)
  		   For a drop list, the first selected value. Use 0 to
		     leave blank}
  \item{horizontal}{A logical specifying the  layout for gradio and gcheckboxgroup}
  \item{handler}{Called when selection is changed}
  \item{action}{Passed to handler when called.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{svalue} method returns the selected value by
	name. If the extra argument \code{index=TRUE} is specified,
	the indices of the selected value is given. 


	The \code{svalue<-} method can be used to set the selected
	value. One can specify the value by name or by index if
	\code{index=TRUE} is specified.

	The \code{"["} method refers to the vector defining the items.

	The \code{"[<-"} method can be used to change the vector
	defining the items. The length will most likely need to be the same.

	The \code{"length"} method returns the number of items.
	
}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{

The radio group is one of several widgets useful to selecting a value
or values from a set of items. See also \code{\link{gcheckbox}},
\code{\link{gcheckboxgroup}}, \code{\link{gcombobox}}, and
\code{\link{gtable}}

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

  w <- gwindow("Radio example")
  gp <- ggroup(container=w)
  glabel("Favorite flavor:",cont=gp)
  rb <- gradio(flavors, cont=gp, handler = f)

  w <- gwindow("combobox example")
  gp <- ggroup(container=w)
  glabel("Favorite flavor:", cont=gp)
  gcombobox(flavors, editable=TRUE, cont=gp, handler=f)

  w <- gwindow("checkbox example")
  gp <- ggroup(container=w)
  glabel("Favorite flavors:",cont=gp)
  cbg <- gcheckboxgroup(flavors, cont=gp, handler=f)

  svalue(cbg) <- c(TRUE, FALSE, TRUE)
  svalue(cbg)
  cbg[3] <- "raspberry"

}

}
\keyword{interface}% at least one, from doc/KEYWORDS
