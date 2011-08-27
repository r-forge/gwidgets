\name{glabel}
\alias{glabel}
\alias{svalue<-,gLabel-method}
%%\alias{rotatelabel}
\title{Constructors for label widget}
\description{
  This constructor produces a widget to display a line or multipline
  lines of text.

  For some toolkits, the text can be marked up.

  An option is available so that the displayed text can be edited.
}
\usage{
glabel(text = "", markup = FALSE, editable = FALSE, handler = NULL,
action = NULL, container = NULL, ..., toolkit = guiToolkit())

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{Text to show in the label or button. For buttons, if
    this text matches a stock icon name, an icon is shown as well}
  \item{markup}{ Logical indicating if text for a label uses markup }
  \item{editable}{Logical. If TRUE, then the label's text can be set
  by clicking on the label and filling in the edit box. }
  \item{handler}{Handler called on a click event}
  \item{action}{ Passed to handler }
  \item{container}{Optional container to attach widget to.}
%  \item{obj}{a \code{glabel} instance}
%  \item{angle}{Angle to rotate label, in degrees}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}

}
\details{
  
	The \code{svalue()} method returns the value of the
	widget. For a label, this is the text as a single string
	(which may not include a "\\n" for newlines if not supported by
	the toolkit). 
	
	
	The \code{svalue<-()} method can be used to set the value of
	the widget. For labels and buttons, value with length greater
	than one are pasted together collapsed with "\\n". 

	The \code{addhandlerclicked} method specifies a handler to be
	called on click events.

	Although in some toolkits, labels are meant to hold static text,
	gWidgets treats label widgets like other widgets allowing the user to
	bind handlers to mouse clicks.  For labels, if
	\code{editable=TRUE} is specified, clicking on the text allows
	one to edit the label's value overriding the click handler in
	the process. However, the \code{addhandlerchanged} handler can
	be given to respond to the text after it has been chnaged.


}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
   glabel("a label", container=TRUE)
   glabel("Click me to edit label", editable=TRUE, container=TRUE)
   glabel("Click me for a message", container=TRUE,
   handler=function(h,...) {cat("Hi\n")})
}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
