\name{gwindow}
\alias{gwindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for base container}
\description{
  Widgets are packed inside containers which may in turn be packed
  inside other containers. The base container is known as a window. Only
  one container may be packed inside a window.
}
\usage{
gwindow(title = "Window", visible = TRUE, name=title,
  width = NULL, height= NULL, parent=NULL,
  handler = NULL, action = NULL,
  ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{Title of window}
  \item{visible}{If \code{TRUE} window is drawn when
  constructed. Otherwise, window can be drawn latter using \code{visible<-}.}
\item{name}{Name for registry of windows}
\item{width}{Default width for window at creation}
\item{height}{Default height for window at creation}
\item{parent}{If non-NULL, can be used to suggest default location of
window. The argument name was changed from location to parent. This can be a coordinate pair (x,y) with (0,0) the upper left
corner, or a gwindow instance. In the latter case the location is
suggested by the location of the current window. This is useful for
placing dialogs near the parent window.}
\item{handler}{Handler for destroy event.}
\item{action}{Passed to handler}
\item{\dots}{Not used}
\item{toolkit}{Which GUI toolkit to use}  
}
\details{
   A base window can also be created using the argument
   \code{container=TRUE} when constructing a widget.

   The \code{svalue} method refers to the window title. Use
   \code{svalue<-} to change the title.

   The \code{add} method is used to add a widget or container to the
   base window. For top-level windows, some toolkits only support adding
   one widter, so in gWidgets only one widget should be added to a
   window, so usually it would be another container.

   Additionally the menubar, toolbar and statusbar widgets should now be
   added and deleted from the top-level window (Not yet implemented in
   gWidgetsrJava). Outside of RGtk2, the other toolkits expect these
   items to be properties of a top-level window.
   
   The \code{dispose} method destroys the window.

   The \code{size} method sets the minimum size. Use the \code{width}
   and \code{height} arguments to set the default size when the window
   is constructed.

   A window is destroyed  in response to a destroy event. However, when
   the window managager tries to close a window first a "delete-event"
   is issued. If this has the right value then the "destroy" event is
   fired. The \code{addHandlerUnrealize} handler can be called to
   intercept the closing of the window. Its handler should return a
   logical: \code{TRUE} to prevent the closing, \code{FALSE} to proceed.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  ## window with handler
  win <- gwindow("Window example",
    handler=function(h,...) {
     print("See ya")
  })
  gbutton("cancel", cont=win,
    handler = function(h,...) dispose(win))

  ## block closing of window
  win <- gwindow("Window example")
  addHandlerUnrealize(win, handler = function(h,...) {
    val <- gconfirm("Really close window", parent=h$obj)
    if(as.logical(val))
      return(FALSE)		# destroy
    else
      return(TRUE)		# don't destroy
  })

  

  ## transient dialog (gWidgetsRGtk2)
  pwin <- gwindow("Parent window")
  cwin <- gwindow("Child window", parent = pwin)
  ## clicking button close parent causing child to close too
  gbutton("close both", cont=cwin,
    handler = function(h,...) dispose(pwin))
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
