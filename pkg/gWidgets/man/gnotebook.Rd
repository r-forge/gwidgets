\name{gnotebook}
\alias{gnotebook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{constructor for notebook widget}
\description{
	A notebook widget organizes different pages using tabs,
	allowing only one page to be shown at once. Clicking on
	the tab raises the associated page.
}
\usage{
gnotebook(tab.pos = 3, closebuttons = FALSE, dontCloseThese = NULL, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab.pos}{Where to place tabs (1 bottom, 2 left side, 3 top, 4
  right side}
  \item{closebuttons}{Is there a close button in the tab?}
  \item{dontCloseThese}{If \code{closebuttons=TRUE} this will make it
  impossible to remove these tabs. Specified by tab number}
  \item{container}{Optional parent container to attach notebook widget to }
  \item{\dots}{passed to \code{add} method of parent container}
  \item{toolkit}{Which GUI toolkit to use}  

}
\details{
  In what follows, it is useful to think of a notebook as a vector with
  named entries, each entry being a widget, the name being the tab label.

  Notebooks have the following methods:

  New pages are added with the \code{add} method, which most likely is
  called by the widget constructor. The extra argument \code{label} is
  used to specify the tab label. This may be a string, or in
  gWidgetsRGtk2 a \code{glabel} instance. The extra argument
  \code{index} can be used to specify which page to add to. By
  default, a new page is created at the end of the notebook. In
  gWidgetsRGtk2, the extra argument \code{override.closebutton} can be
  used to add or not add a close button in the tab label.

  The \code{svalue} method returns the current page number. The
  \code{svalue<-} method is used to set the page number.

  The \code{dispose} method will remove the currently selected page
  unless it is overridden by the value of \code{dontCloseThese}.

  The \code{delete(obj, widget,...)} method will delete the widget on a
  given page. For some toolkits, the unparented widget can be reparented with the \code{add} method or \code{[<-}.

  The \code{length} method returns the number of pages.

  The \code{names} method returns the tab labels.

  The \code{names<-} method may be used to replace the tab
  labels. Something like \code{names(obj)[1]<-"new label"} should work.

   The \code{"["} method refers to the widgets in the notebook. It
   returns a single widget or list of widgets.

   For some toolkits, the \code{"[<-"} method may be used to replace a widget on a notebook page.
  
}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{See \code{\link{gwindow}} for top-level containers, 
  \code{\link{ggroup}}, \code{\link{gframe}} and \code{\link{gexpandgroup}} for box containers}

\examples{
\dontrun{
  w <- gwindow("gnotebook example")
  nb <- gnotebook(container=w)
  glabel("Widget 1", cont=nb, label="page 1")   ## "add" called by constructor
  glabel("Widget 2", cont=nb, label="page 2")   ## label argument passed by constructor to add method

  length(nb)
  names(nb)
  names(nb)[1] <- "Page 1"

  svalue(nb) <- 2
  dispose(nb) ## dispose current tab
  length(nb) 
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
