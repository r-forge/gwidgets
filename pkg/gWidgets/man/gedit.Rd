\name{gedit}
\alias{gedit}
\title{Constructor for widget to handle single-line text input}
\description{
	The \code{gedit} widget is used to enter single lines of text. 
	}
\usage{
gedit(text = "", width = 25, coerce.with = NULL, initial.msg="", handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{text}{Initial text in widget}
  \item{width}{Width of widget. For gedit, this means the number of
    characters.}
  \item{coerce.with}{For gedit, when the value is retrieved this
    function is applied to the result. (The stored value is always a
    character, this can be used to make it numerc, to quote it, ...}
    \item{initial.msg}{If \code{text} is empty, this initial message is displayed to give the user some indication of what to do}
  \item{handler}{Handler called when text is changed. For gedit, this
    means the enter key is pressed.}
  \item{action}{ Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to add method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{gedit} widget has the following methods:

	The \code{svalue} method retrieves the value. If a function is
	given to the argument \code{coerce.with} it is applied before
	the value is returned. This can be used to coerce the text
	value (always of class character) to a numeric, or to a date,
	or to be quoted, ...

	The \code{svalue<-} method is used to set the value.

	The \code{"["} and \code{"[<-"} methods refer to the widgets
	"type-ahead" values. A familiar usage is when a url is typed
	into a web browser, matches appear from a users history that
	could possibly complete the typed url.
	
	The \code{visible<-} method is used to toggle whether
	characters are visible, or are replaced with a "*", such as is
	done with password entry.

      }
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
	gedit("type here", container=gwindow())
	
	## change handler
	obj <- gedit(container=gwindow())
	addhandlerchanged(obj, handler=function(h,...) 
	  cat("You typed", svalue(h$obj),"\n"))

	 ## coerce to numeric
	 obj <- gedit("7", container=gwindow(), coerce.with=as.numeric)
	 svalue(obj)

}
}
\keyword{interface }
