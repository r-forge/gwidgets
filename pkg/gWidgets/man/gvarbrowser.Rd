\name{gvarbrowser}
\alias{gvarbrowser}
\title{Widget for browsing environment}
\description{
  A widget to browse the objects in the current global environment
}
\usage{
gvarbrowser(handler = NULL,
                 action = "summary", container = NULL, ...,
                 toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{handler}{Handler for double click. Default is to call value of
    \code{action} on the object}
  \item{action}{Passed to handler.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ignored}
  \item{toolkit}{Which GUI toolkit to use}  
}
\details{
  In gWidgetsRGtk2 there is an idle handler that updates the top-level
  components. However, changes are made below this, they will not be
  updated automatically. Rather, the user must close and expand that
  level.

  The handler is called on the widget that provides the display, and not
  the gvarbrowser widget. If you want that in the handler, be sure to
  pass it in via the action argument.

  svalue returns a character string containing the selected variable.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
gvarbrowser(container=TRUE)
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
