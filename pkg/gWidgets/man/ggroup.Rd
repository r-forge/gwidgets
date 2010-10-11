\name{ggroup}
\alias{ggroup}
\alias{gframe}
\alias{gexpandgroup}
\title{Containers for packing in subsequent widgets}
\description{

	Various box containers useful for laying out GUI controls. These
	containers pack in child widgets from left to right or top to
	bottom. A few arguments can be used to adjust the sizing and positioning.

}
\usage{
ggroup(horizontal = TRUE, spacing = 5,  use.scrollwindow = FALSE, container = NULL, ..., toolkit = guiToolkit())

gframe(text = "", markup = FALSE, pos = 0, horizontal=TRUE, container = NULL, 
    ..., toolkit = guiToolkit()) 
gexpandgroup(text = "", markup = FALSE, horizontal=TRUE, handler = NULL, action = NULL, 
    container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{horizontal}{Specifies if widgets are packed in left to right
	or top to bottom (\code{FALSE})}
  \item{spacing}{Space in pixels around each widget. Can be changed
	with \code{svalue}}
  \item{text}{Text for label}
  \item{markup}{Optional markup. (See \code{\link{glabel}} for details.)}
  \item{pos}{Where to place label: 0 is to left, 1 to right,
  interpolates.}
  \item{handler}{Called when expand arrow is clicked}
  \item{action}{Passed to handler}
  \item{use.scrollwindow}{If \code{TRUE} then group is placed in a
  scrollwindow allowing panning with mouse.}
  \item{container}{Optional container to attach widget to. Not optional
    for gWidgetstcltk, or gWidgetsRwxwidgets}
  \item{\dots}{Passed to the \code{add} method of the container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{


	A \code{ggroup} is the primary container for packing in
	subsequent widgets, either from left to right or top to
	bottom. Widgets are packed in using the \code{add} method and
	can be removed with the \code{delete} method.
	
	The \code{gframe} container adds a decorative border and
	optional label to the box container.

	The \code{gexpandgroup} containers has an optional label and a
	trigger to click  on which toggles the display of the the child widgets.
	By default, the child widgets are not shown. Using the \code{visible<-} method to adjust.
  
	The containers pack in child widgets from either left to right
	(when \code{horizontal=TRUE}) or from top to bottom (when
	\code{horizontal=FALSE}).
	
	Child widgets are added to box containers through their \code{add}
	method or through the containers use as the parent container
	when a widget is constructed. This is done by using the
	\code{container} argument of the widget.	
	The \code{container} argument is optional for
	\code{gWidgetsRGtk2} and \code{gWidgetsrJava}, but not the other
	toolkits. It is suggested that it always be included for
	portability. When it is not included, widgets are added to the
	new group object through its add method. Otherwise, when a
	widget is created, the group is specified as the container and
	the add method is then called.
	
	When the parent allocates space to a child widget is may be more
	than the space needed by the child widget. Different types of
	behavior are desirable for drawing the child. To have the widget
	grow to fill the space, use the argument
	\code{expand=TRUE}. (Growing is done in both x and y
	directions. To anchor the widget into a portion of the space
	provided, use the argument \code{anchor=c(a,b)} where \code{a}
	and \code{b} are values in -1,0,1 and specify the position using
	Cartesian coordinates.

	The \code{svalue<-} method can be used to adjust the border
	width. By default it is 2 pixels.
	
	The \code{spacing} value determines the number of pixels of
	padding between each widget when packed in. This can be set when
	the group is constructed, or later using \code{svalue<-}.

	To put  space between just two widgets, the
	\code{addSpace(obj, value, ...)}
	method may be used where \code{value} is the number of pixels
	of padding between the just packed widget, and the next one to
	be packed.


	The \code{addSpring(obj,...)} method will push the just packed
	widget and the next-to-be packed widget as far apart as possible.
	

	For \code{ggroup}, in \code{gWidgetsRGtk2} a few arguments add
	to the container.  The argument \code{raise.on.dragmotion =
	TRUE} will cause the group to jump to the foreground when a drag
	action crosses it.  The argument \code{use.scrollwindow = TRUE}
	will put the group
	
	For \code{gframe} and \code{gexpandgroup} the label name can be
	retrieved or adjusted with the \code{names} method.

	For \code{gexpandgroup} the \code{visible} method can be used
	to toggle the display programmatically.

	
      }
% \value{}
% \references{}
% \author{}
% \note{ }
\seealso{For top-level containers \code{\link{gwindow}}, for containers
  to display more than one child widget see \code{\link{gpanedgroup}},
  \code{\link{gnotebook}}, \code{\link{glayout}}}
\examples{
\dontrun{
  ## basic group 
  group <- ggroup(horizontal=FALSE, container=gwindow())
  l <- glabel("widget 1")  ## not in gWidgetstcltk -- needs a container
  add(group, l)
  glabel("widget 2", container = group) ## style for all toolkits

  ## nested groups
  group <- ggroup(horizontal=FALSE, container=gwindow())
  innergroup <- ggroup(container = group)
  gtext("Text area", container=group)
  gbutton("button 1", container = innergroup)
  gbutton("button 2", container = innergroup)

  ## expand argument
  group <- ggroup(horizontal=FALSE, container=gwindow())
  gbutton("no expand", container=group)
  gbutton("expand=TRUE", container=group, expand=TRUE)

  ## anchor argument
  w <- gwindow("Anchor")
  size(w) <- c(500,500)
  group <- ggroup(container=w)
  glabel("upper left", container=group, anchor=c(-1,1))
  glabel("lower right", container=group, anchor=c(1,-1))

  ## add spring
  group <- ggroup(container=gwindow("menubar-like example"))
  gbutton("File",handler=function(h,...) print("file"), container=group)
  gbutton("Edit",handler=function(h,...) print("edit"), container=group)
  addSpring(group)				   
  gbutton("Help",handler=function(h,...) print("help"), container=group)

}
}
\keyword{ interface }

