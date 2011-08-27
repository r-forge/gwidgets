\name{gradio}
\alias{gradio}
\alias{[<-,gRadio-method}
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
  \item{selected}{ The initial selected value (as an index). Radio groups must have a selection}
  \item{horizontal}{A logical specifying the  layout for gradio}
  \item{handler}{Called when selection is changed}
  \item{action}{Passed to handler when called.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{svalue} method returns the selected value by
	name. If the extra argument \code{index=TRUE} is specified,
	the index of the selected value is given. 


	The \code{svalue<-} method can be used to set the selected
	value. One can specify the value by name or by index if
	\code{index=TRUE} is specified.

	The \code{"["} method refers to the vector defining the items.

	The \code{"[<-"} method can be used to change the vector
	defining the items. 

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

  w <- gwindow("Radio example", visible=FALSE)
  gp <- ggroup(container=w)
  glabel("Favorite flavor:",cont=gp, anchor=c(0,1))
  rb <- gradio(flavors, cont=gp)
  addHandlerClicked(rb, handler=function(h,..) {
   cat(sprintf("You picked \%s\n", svalue(h$obj)))
  })
  visible(w) <- TRUE
  
  betterFlavors <- c("coffee", "mint chip")
  rb[] <- betterFlavors 
  rb[] <- c(betterFlavors, "chocolate") # some toolkits don't allow change of length
  
  rb[3] <- "mango sorbet" ## can change a label name

  ## set values
  svalue(rb) <- "coffee"  ## by name
  svalue(rb, index=TRUE) <- 1  ## by index

  ## get selected values
  svalue(rb)
  svalue(rb, index=TRUE)

}

}
\keyword{interface}% at least one, from doc/KEYWORDS
