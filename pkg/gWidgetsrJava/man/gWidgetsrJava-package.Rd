\name{gWidgetsrJava-package}
\alias{gWidgetsrJava-package}
\alias{gWidgetsrJava}
\docType{package}
\title{
Toolkit implementation of gWidgets for rJava
}
\description{
Port of gWidgets API to rJava. The gWidgets API is an abstract,
lightweight means to interact with GUI toolkits. In this case, the JAVA toolkit.
}
\details{
This file documents differences between gWidgetsrJava and the gWidgets
API. Currently, gWidgetsrJava only provides a no-frills version of
gWidgets, as compared to gWidgetsRGtk2.

Notes on this implementation:

First, although the widgets can be constructed from the R commandline,
they can only be given handlers (or a response to mouse events) if the
package is run with the \pkg{JGR} environment. This is a limitation of
the \pkg{rJava} package which provides the interface to the Java
libraries from within R. 
\cr

\bold{Containers:}

To speed up the display of complicated GUIs, the \code{visible=FALSE}
argument to the \code{gwindow} constructor can be used to suppress the
showing of the window and its child components. The method
\code{visible} can then be used to show the GUI.

\code{gframe()} no markup in title
\cr

\code{glayout()} has two additional arguments: \code{expand=TRUE} is like \code{expand=} for \code{ggroup()}, in
that the attached widget expands to fill the possible space in the
container. If this isn't given the anchor= argument can be used to
adjust the location of the widget withing the cell. A value of c(-1,1)
is the lower left, c(-1,1) the upper left (the default), c(1,-1) the
lower right, and c(1,1) the upper right. The value 0 for either is also
possible.
\cr

\code{ggroup()} also has the \code{expand=} and \code{anchor=} arguments. If
\code{expand=TRUE} the widget
takes up as much space as possible. The \code{anchor=} argument adjusts a widget
left or right, up or down, within its space. Only one component
works at a time for the anchor argument. In a horizontal box, only the y
component can be used to move a value up or down. In a vertical box,
only the x component can be used to move a value left or right. The
default is c(-1,1) so that horizontal layouts are towards the top, and
vertical layouts towards the left.
\cr

\bold{Components:}
\cr
\code{gcalendar()} is mostly  implemented. It uses a calendar widget from
\url{http://java.sun.com/developer/JDCTechTips/2005/tt0601.html}, a now
defunct project. The return value of \code{svalue()} is a \code{Date} object, not a
character string, formatted by the \code{format=} string.
\cr

\code{gcommandline()} is implemented, but could definitely be
improved. The console in JGR is much better.
\cr


\code{gdf()} works, looks bad. It is \emph{slow}
\cr

\code{gdfnotebook()} needs to be changed to run. Not implemented
\cr

\code{gedit()}, unlike the \pkg{gWidgetsRGtk2} widget, has no
type-ahead support via drop down boxes.  The method \code{addHandlerKeystroke} does not have the key returned through the \code{h\$key} component.\cr

%\code{ggraphics()} uses the one from \pkg{JGR} (via \pkg{JavaGD}). This
%means no graphics notebook and no embedding of graphics devices.
%\cr

\code{ghelpbrowser()} just uses the one from \pkg{JGR},  its methods are not implemented.
\cr

\code{gimage()} size argument is ignored. It seems that images get
cached. If you try something like \code{svalue(gimageInstance)<-file;}
\code{update(file);} \code{svalue(gimageInstance)<-file} then the
updated file won't be loaded.
\cr

\code{gfile()} no filtering is implemented.
\cr

\code{gmenu()} adds only to the top window, not any container. This is a Java
limitation. Use a popupmenu instead. 
\cr

\code{gstatusbar()} is just a \code{glabel} instance in disguise. 

\code{gradio()} has an extra argument \code{coerce.with=}, as otherwise it would treat
everything as a character vector. It tries to guess when instantiated,
if not explicitly given.
\cr

\code{gslider()} in \pkg{rJava} works with integer steps only. If the \code{by=} argument is
less than 1 a \code{gspinbutton()} is used in place.
\cr

\bold{Handlers:}
\cr

There is no support for \code{adddropsource()}, \code{adddroptarget()} and
\code{adddropmotion()}. Drag and drop basically works as typical, you drag it
and it adds where dropped.


}
\author{
John Verzani, although based on the \pkg{iWidgets} package of Simon Urbanek,
incorporating some GPL and LGPL software from the eclipse project, the
javadesktop.org project, the scigraphics project and code from the Java
Tutorial by SUN.

Maintainer: John Verzani <gwidgetsrgtk@gmail.com>
}
\references{
  See JGR's website at \url{www.rosuda.org}.
}
\note{
  
This package uses Java source code from several GPL projects:
xnap-common, the Java tutorial, iWidgets, javadesktop.org.

}
\keyword{ package }
\seealso{The \pkg{JGR} package: \url{www.rosuda.org}}
\examples{
\dontrun{
##
options(guiToolkit="rJava")
## select CRAN miror example
  win = gwindow("Select a CRAN mirror")
  size(win) = c(600,400)
  tbl = gtable(utils:::getCRANmirrors(), 
  container=win,
  chosencol=4, 
  handler = function(h,...) {
    URL = svalue(h$obj)
    repos = getOption("repos")
    repos["CRAN"] = gsub("/$", "", URL[1])
    options(repos = repos)
    dispose(win)
  })
##
}
}
