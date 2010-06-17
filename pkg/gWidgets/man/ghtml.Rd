\name{ghtml}
\alias{ghtml}
\title{Constructors for widgets to handle text input}
\description{
  The ghtml widget is intended to show HTML text either from a url or
  from a character string. Currently no toolkits support this widget, although it is in gWidgetsWWW.
}
\usage{
ghtml(x, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

}
\arguments{
  \item{x}{url or HTML-marked up character string to load into widget.}
  \item{handler}{Handles a click on a URL. The default is to open the
     clicked url in the widget. To override, the first argument,
     a list \code{h}, has component \code{h\$url} containing the url.}
  \item{action}{ Passed along to the  handler as \code{h\$action}}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  This widget loads the given url into a widget. Currently no toolkits
  support this.

  The \code{svalue} method returns the current url or character string.

  The \code{svalue<-} method loads the current url or character string
  in the widget.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  ghtml(system.file("html","gedit.html",package="gWidgets"),
    container = gwindow())
}
}
\keyword{interface }
