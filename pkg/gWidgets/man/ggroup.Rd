\name{ggroup}
\alias{ggroup}
\alias{gframe}
\alias{gexpandgroup}
\title{Box containers for packing in subsequent widgets}
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
	
	Child widgets are added to box containers through their
	\code{add} method or through the containers use as the parent
	container when a widget is constructed. This is done by using
	the \code{container} argument of the widget.  The
	\code{container} argument is not optional for
	\pkg{gWidgetstcltk}.  It is suggested that it always be included
	for portability. When it is not included, widgets are added to
	the new group object through its \code{add} method. Otherwise, when a
	widget is created, the group is specified as the container and
	the add method is then called implicitly, with the constructor's
	\code{...} argument used to pass arguments to \code{add}.
	
	When the parent allocates space to a child widget it
	be possible to allocate more space than is requested by the
	child widget. The child may then be
	positioned in the available space by specifying the
	\code{anchor=c(a,b)} argument to \code{add} where \code{a}
	and \code{b} are values in -1,0,1 and specify the position using
	Cartesian coordinates.

	If the argument \code{expand=TRUE} to \code{add} is given, then the space
	available to the child expands.  The child widget can be
	instructed to grow to fill the space. The \code{add} method's argument \code{fill},
	with values \code{"both"}, \code{"x"}, \code{"y"} or \code{""},
	instructs the child as to which direction to grow in. (The
	\code{""} value says none.)

	The implementation of \code{expand}, \code{anchor}, \code{fill}
	varies with the underlying toolkit. The basic language comes
	from \pkg{tcltk} and an attempt -- not entirely successful -- is
	made to implement it in the \pkg{gWidgetsRGtk2} and
	\pkg{gWidgetsQt} packages.

	A child component may be deleted using \code{delete(parent,
	child)}. TYpically the child may be replaced in the GUI, using \code{add}.
	
	The \code{spacing} argument determines the number of pixels of
	padding between each widget when packed in. This can be set when
	the group is constructed, or later using \code{svalue<-}.

	The argument \code{use.scrollwindow = TRUE}
	will add scrollbars around the box container. When the child
	components require more size than is given to the container, the
	scroll bars allow one to position the viewable area over the
	child of interest. Again, not all toolkits do this equally well.
	
	
	To put space between two adjoining widgets, the
	\code{addSpace(obj, value, ...)}  method may be used where
	\code{value} is the number of pixels of padding between the just
	packed widget, and the next one to be packed.


	The \code{addSpring(obj,...)} method will push the just packed
	widget and the next-to-be packed widget as far apart as possible.
	

	For \code{ggroup}, in \code{gWidgetsRGtk2} a few arguments add
	to the container.  The argument \code{raise.on.dragmotion =
	TRUE} will cause the group to jump to the foreground when a drag
	action crosses it.
	For \code{gframe} and \code{gexpandgroup} the label name can be
	retrieved or adjusted with the \code{names} method.

	For \code{gframe} and \code{gexpandgroup} the label can be
	adjusted through the \code{names<-} method.
	
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


  ## delete and add
  w <- gwindow("Delete and add", visible=FALSE)
  g <- ggroup(cont=w)
  b <- gcheckbox("hide", checked=FALSE, cont=g)
  l <- gedit("click checkbox to hide me", cont=g)
  addHandlerClicked(b, handler=function(h,...) {
     if(svalue(b))
       delete(g, l)
     else
       add(g, l)
  })
  visible(w) <- TRUE
}
}
\keyword{ interface }

