\name{gWidgets-dnd}
\alias{gWidgets-dnd}
\alias{adddroptarget}
\alias{adddropmotion}
\alias{adddropsource}
\alias{addDropTarget}
\alias{addDropMotion}
\alias{addDropSource}


\title{Functions to add drag and drop ability to widgets}
\description{
	These functions allow drag and drop between widgets. The basic
	idea is that one creates drop sources from which one defines
	values which may be
	dragged and drop target where values may be dropped. These
	values can be text, or widgets.
}
\usage{
adddropsource (obj, targetType = "text", handler = NULL, action = NULL, 
    ...) 
adddropmotion (obj, handler = NULL, action = NULL, ...) 
adddroptarget (obj, targetType = "text", handler = NULL, action = NULL, 
    ...) 
}
\arguments{
	\item{obj}{Object to put drop handler on}
	\item{targetType}{What type of drop target: either "text" or
	"pixmap" or "entry".}
	\item{handler}{Handler called for the drop motion}
	\item{action}{action passed to handler}
	\item{...}{Not documented, currently has no role.}
}
\details{
	To specify if one can drag values from a widget use
	\code{adddropsource} called on the object.  The argument
	\code{targetType} can be set to \code{"object"} when the drop
	value is to be a widget, and not a string.  The arguments
	\code{handler} and \code{action} can be used to describe what
	gets dropped. The default is to drop the widget's contents as
	returned by \code{svalue}.

	To specify if an object is a drop target the
	\code{adddroptarget} method is called on the object. The
	argument \code{handler} (but no \code{action}) is used to handle
	the drop.

	The handler's first argument is a list with named components.
	The \code{obj} component refers to the object that has the
	target placed on it. The component \code{dropdata} is set by the
	\code{adddropsource} method. The dropdata is typically a string,
	but a mechanism is in place to drop widgets.
	The default handler for \code{adddroptarget} is to replace the
	widget's value with the dropped data.

	To add an action to a motion event, use the
	\code{adddropmotion} method. The \code{adddroptarget} must first
	have been added to the object.
}
\value{
	These functions return an ID returned  when registering
	a handler. The function \code{removehandler} uses this
	information to remove a drag and drop handler.
}
\author{Implementation of Simon Urbanek's iwidgets API was done by
Michael Lawrence and John Verzani }
\seealso{\link{gWidgets-methods}} 
\examples{
\dontrun{
## simple dnd
lab  = glabel("drag me",container=gwindow())
ed = gedit("drop here",container = gwindow())
adddropsource(lab)
adddroptarget(ed)
adddropmotion(ed,handler=function(h,...) print("bombs away"))

## more complicated
## this shows that rows of editable data frame can be dropped.
## by assigning to the changed signal, the graphs can be dynamic.
## THat is, drop a column, then edit it. The graph will update. The key
## is referring to the "value" stored in gd. This refers to the column
## in the editable data frame.
## By using svalue() and id(), the dropped value can also be a
## character string referring to a variable in the workspace.
adf = gdf(mtcars, cont = TRUE)
gd = ggraphics(cont = TRUE)
plotData = function() {
  dropvalue = tag(gd,"value")
  theValues = svalue(dropvalue)
  theName = id(dropvalue)
  hist(theValues, xlab=theName, main="")
}

ids = adddroptarget(gd,targetType="object", handler = function(h,...) {
    tag(gd, "value") <- h$dropdata
    plotData()

    if(is.gdataframecolumn(h$dropdata)) {
      view.col = h$dropdata
      id = addhandlerchanged(view.col, handler=function(h,...) plotData())
    }
})
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
