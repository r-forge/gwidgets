\name{gcheckbox}
\alias{gcheckbox}
\title{

Constructor of widget to indicate whether a value is desired or not

}
\description{
	A checkbox shows a value and a box to  check indicating if the
	value is desired or not.
}
\usage{
gcheckbox(text, checked = FALSE, handler = NULL, action = NULL,
container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{Text to show by box}
  \item{checked}{Logical indicating initial state of box}
  \item{handler}{ Called when box is toggled.}
  \item{action}{ Passed to handler}
  \item{container}{ Optional container to attach widget to.}
    \item{...}{Not documented, currently has no role.}
  \item{toolkit}{Which toolkit to use?}
}
\details{
	The value of the widget is either \code{TRUE} or \code{FALSE}.

	The \code{svalue} method returns a logical indicating
	\code{TRUE} if the box is checked.

	The \code{svalue<-} method can be used to set the value
	using a logical.

	The \code{"["} method returns the label on the box.

	The \code{"[<-"} method can be used to change the label on the
	box.

	The default handler is set by the \code{addHandlerClicked}
	method. This is called when the button is 
	toggled. If one wishes to have the handler called only when
	checked to indicate \code{TRUE}, say, one should check the state
	of the widget in the handler.
}
%%\value{}
%%\references{}
\author{John Verzani}
%%\note{}
\seealso{

Methods for gComponent objects are detailed in \code{\link{gWidgets-methods}}.

Event Handlers are detailed in \code{\link{gWidgets-handlers}}.


}
\examples{
\dontrun{
 gcheckbox("checked", container=TRUE, handler=function(h,...) {
   cat("The widget is checked?",svalue(h$obj), "\n")
})
}
}
\keyword{ interface }
