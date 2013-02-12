\name{gformlayout}
\alias{gformlayout}

\title{A constructor for laying out groups of widgets from a template defined by a list}
\description{
	This constructor takes a list that defines the layout of widgets
	and pieces them together to create a form or dialog. It is
	similar to \code{ggenericwidget} but offers more flexibility
	with the layout, but does not offer the automatic creation of
	the widget using a functions \code{formals}. 
}
\usage{
gformlayout(lst,  container = NULL, ..., toolkit = guiToolkit())
}

\arguments{
  \item{lst}{A list that defines the layout of the containers. See the
  details section.}
  \item{container}{Optional parent container for the widget}
  \item{\dots}{Passed to \code{add} method of parent container when given}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The list defining the layout has the following key named components:

	\describe{
	  \item{type}{The type is the name of a gWidgets constructor or
	    "fieldset". The latter specifies that the children should be
	    layed out using a table. The type can specify either a container
	    constructor or component contstructor}
	  \item{children}{For containers, this specifies the children
	  using a
	    list. Each child is a given as a component of this
	    list. Children can be containers and hence contain other
	    children, to match the heirarchical layout common in GUIs.}
	  \item{name}{If a name is specified, then this widget will be
	    stored in a list that can be accessed by the methods
	    \code{svalue} or \code{\[}}
	  \item{depends.on}{The name of a widget previously specified
	    through the \code{name} argument. If given, then a handler is
	    added to the widget that controls whether this new
	    widget/container should be enabled.}
	  \item{depends.FUN}{When \code{depends.on} is specified, this
	    function is consulted to see if the widget should be
	    enabled. This function has argument \code{value} which is
	    the return value of \code{svalue} on the named widget this
	    new one depends on. It should return a logical value
	    indicating if the new widget is to be enabled.}
	  \item{depends.signal}{By default, the signal the handler
	    specified through \code{depends.FUN} is given by
	    \code{addHandlerChanged}, this allows on to specify a
	    different \code{addHandler} method. See the example.}
	}
	
	If the type is  \code{gnotebook}, then each child should have a
	\code{label} component.
	
	The new constructor \code{fieldset} allows the organization of
	its children in a table. These children should not be other
	containers. If the component \code{label} is non-null, the table is
	packed into a \code{gframe} container. The default number of
	columns is just 1, but specifying \code{columns} in the list can
	increase this. Children are packed in row by row when more than
	one column is given.

	The labels can be adjusted. The component \code{label.pos} can
	be "left" (the default) for a label to the left of the widget,
	or "top" to have the label on top of the widget. When the
	position if "left", then the \code{label.just} component is
	consulted for justification of the label. This can have a value
	of "right" (the default), "center" or "left"

	If a component \code{label.font} is given, then this will be
	applied to each label through the \code{font} method of the
	label.

	The children are specified as a list. Each child should have a
	\code{type} component, a \code{label} component and a
	\code{name} component. Other components are passed to the
	specified constructor in type through \code{do.call}.

	The return object has a few methods defined for it.
	
	The \code{\[} method returns a list with named components storing
	the objects created by the constructors. Subsetting is
	allowed. No \code{\[\[} method is given, instead the
	\code{drop=TRUE} argument can be used with a single index is
	given to return the component and not the list containing the component.
	
	The \code{svalue} method is a convenience method that applies
	the \code{svalue} method to each component of the list returned
	by \code{\[}.

	The \code{names} method is a convenience method that gives the
	names of the widgets store in the list returned by \code{\[}.
	
      }
      
% \value{}
% \references{}
% \author{}
\note{The design of this borrows from the FormPanel and FormLayout
  constructors in the 
  \url{extjs.com} library for javascript programming.}
\seealso{\code{\link{ggenericwidget}}}
\examples{
\dontrun{

## layout a collection of widgets to generate a t.test
tTest <- list(type = "ggroup",
              horizontal = FALSE,
              children = list(
                list(type="fieldset",
                     columns = 2,
                     label = "Variable(s)",
                     label.pos = "top",
                     label.font = c(weight="bold"),
                     children = list(
                       list(name = "x",
                            label = "x",
                            type = "gedit",
                            text = ""),
                       list(name = "y",
                            label = "y",
                            type = "gedit",
                            text = "",
                            depends.on = "x",
                            depends.FUN = function(value) nchar(value) > 0,
                            depends.signal = "addHandlerBlur"
                            )
                       )
                     ),
                list(type = "fieldset",
                     label = "Hypotheses",
                     columns = 2, 
                     children = list(
                       list(name = "mu",
                            type = "gedit",                            
                            label = "Ho: mu=",
                            text = "0",
                            coerce.with = as.numeric),
                       list(name = "alternative",
                            type="gcombobox",
                            label = "HA: ",
                            items = c("two.sided","less","greater")
                            )
                       )
                     ),
                list(type = "fieldset",
                     label = "two sample test",
                     columns = 2,
                     depends.on = "y",
                     depends.FUN = function(value) nchar(value) > 0,
                     depends.signal = "addHandlerBlur",                     
                     children = list(
                       list(name = "paired",
                            label = "paired samples",
                            type = "gcombobox",
                            items = c(FALSE, TRUE)
                            ),
                       list(name = "var.equal",
                            label = "assume equal var",
                            type = "gcombobox",
                            items = c(FALSE, TRUE)
                            )
                       )
                     ),
                list(type = "fieldset",
                     columns = 1,
                     children = list(
                       list(name = "conf.level",
                            label = "confidence level",
                            type = "gedit",
                            text = "0.95",
                            coerce.with = as.numeric)
                       )
                     )
                )
              )

w <- gwindow("t.test")
g <- ggroup(horizontal = FALSE, container = w)
fl <- gformlayout(tTest, container = g, expand=TRUE)
bg <- ggroup(container = g)
addSpring(bg)
b <- gbutton("run t.test", container = bg)
addHandlerChanged(b, function(h,...) {
  out <- svalue(fl)
  out$x <- svalue(out$x) # turn text string into numbers via get()
  if(out$y == "") {
    out$y <- out$paired <- NULL 
  } else {
   out$y <- svalue(out$y)
  }
  print(do.call("t.test",out))
})
}		
}
\keyword{interface}
