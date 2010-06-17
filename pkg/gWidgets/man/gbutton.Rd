\name{gbutton}
\alias{gbutton}
\title{Button constructors}
\description{
	A button widget is used to present a widget that a user can
	press to initiate some action.

	Buttons show text and/or images in a clickable object whose
	shading indicates that the button is to clicked on.
}
\usage{
gbutton(text = "", border=TRUE, handler = NULL, action = NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{Text to show in the button. For buttons, if
    this text matches a stock icon name, an icon is shown as well. }
  \item{border}{If \code{TRUE} a border is drawn to make a button look like a
  button. If \code{FALSE}, the no border so the button looks like a label.}
  \item{handler}{Handler called on a click event}

  \item{action}{Either a \code{gaction} instance in which case, the
  \code{text} and \code{handler} arguments are not used, or is an R
  object to be passed to the specified handler.

}
  \item{container}{Optional container to attach widget to.}
%  \item{obj}{a \code{glabel} instance}
%  \item{angle}{Angle to rotate label, in degrees}
  \item{\dots}{Passed to \code{add} method of container}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{

  As buttons are intended to show the user how to initiate some action,
  they are often labeled as commands. Additionally, if the action is not
  currently possible given the state of the GUI, a button is typically
  disabled. This can be done through the \code{enabled} method.
  
  The \code{svalue()} method returns the value of the
  widget. For a button, this is the text as a single string
  (which may not include a "\\n" for newlines if not supported by
  the toolkit). 
	
	
  The \code{svalue<-} method can be used to set the text of
  the widget. For buttons, values with length greater
  than one are pasted together collapsed with "\\n".
  
  The \code{addHandlerChanged} method is aliased to the
  \code{addHandlerClicked} method which can be used to set a
  handler to respond to click events.
  
  When the \code{action} argument is a \code{gaction} instance, then
  the button label and handler will be derived from the \code{gaction}
  instance. The \code{enabled<-} method of the \code{gaction} instance
  should be used to set the sensitivity to user input, not the
  \code{enabled<-} method of the \code{gbutton} instance.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  ## button group example
  w <- gwindow("Button examples")
  g <- ggroup(cont = w)
  addSpring(g)   ## push to right of widget
  gbutton("help", cont = g)
  addSpace(g, 20) ## some breathing room
  gbutton("cancel", cont = g)
  gbutton("ok", cont = g, handler = function(h, ...) cat("do it\n"))
}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
