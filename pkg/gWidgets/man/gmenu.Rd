\name{gmenu}
\alias{gaction}
\alias{gmenu}
\alias{gtoolbar}
\title{ Constructors to make menubars or toolbars}
\description{
	A menubar or toolbar are created using these
	constructors. These are specified using a lists, and these may
	be seen as simply mapping these lists into the corresponding widget.
}
\usage{
gmenu(menulist,  popup = FALSE, action=NULL, container = NULL, ..., toolkit = guiToolkit())

gtoolbar (toolbarlist, style = c("both", "icons", "text",
	 "both-horiz"), 
	 action=NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 

gaction(label, tooltip = NULL, icon = NULL, key.accel = NULL, 
        handler = NULL, action = NULL, parent=NULL, ...,
        toolkit = guiToolkit())

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{menulist}{A list defining a menu bar}
  \item{popup}{Logical indicating if this should return a popup menu}
  \item{toolbarlist}{A list defining a toolbar}
  \item{style}{What style to use}
  \item{action}{Passed to menubar handlers}
  \item{container}{Container to attach widget to. Should be a gwindow instance.}
  \item{label}{Label for action item}
  \item{tooltip}{tooltip}
  \item{icon}{icon to decorate instance of action}
  \item{key.accel}{keyboard accelerator. If specified, a parent window must also be specified.}
  \item{parent}{Needed if key.accel is specified}
  \item{handler}{Handler called when object attached to action is activated}
  \item{\dots}{Passed to the \code{add} method of the container}
  \item{toolkit}{Which GUI toolkit to use}

}
\details{

	The gaction constructor creates reusable objects for use with
	buttons, menubars and toolbars. Once constructed, the main
	methods are \code{svalue}, and \code{svalue<-} for getting and
	setting the label text and \code{enabled<-} which can changes
	whether the widgets depending on the action are sensitive to
	user input. An action object contains a label, an optional
	icon, an optional keyboard accelerator, a handler and a parent
	window. The handler does not have the widget from which it is
	called passed in to the \code{obj} component of the first
	argument, but one can parametrize the argument with the
	\code{action} argument. The icon, tooltip, and keyboard
	accelerator are very much toolkit and OS dependent, and so may
	not always be available by the widget using the gaction
	object. The keyboard accelerator may use modifiers
	\code{Control}, \code{Alt} or \code{Shift} along with a
	letter, such as \code{Control-c}.  For \pkg{gWidgetstcltk} the
	value is passed to \code{tkbind}.  For \pkg{gWidgetsQt} the
	value is passed to \code{Qt$QKeySequence}.  For
	\pkg{gWidgetsRGtk2} the value is parsed and manipulated.  The
	keyboard accelerator requires a parent container so that the
	corresponding window for which the accelerator applies can be
	found.

	The lists defining a menubar or toolbar are very similar.

	Each is a list with \emph{named} components. A component is a
	terminal node if it a) is a \code{gaction} instance or b) has a
	\code{handler} component, which is a function to be called
	(without arguments) when the menu item or toolbar item is
	selected. Optionally, an \code{icon} component can be given
	specifying a stock icon to accompany the text. A non-null
	component named \code{separator} will also indicate a terminal
	node. In this case, a visible separator will be displayed.

	A menubar list can use the hierarchical nature of a list to
	generate submenus. For toolbars this is not the case.

	These constructors map the list into the widget. The methods
	for the constructors refer to these list defining the widget.

	The \code{svalue} method returns the list.

	The \code{svalue<-} method can be used to change the list, and
	hence redo the menubar.

	The \code{"["} method refers to the components of the list.

	The \code{"[<-"} method can be used to change pieces of the
	menubar or toolbar.

	The \code{add} method with signature \code{(obj,lst)} or
	\code{(obj,gmenu.instance)} can be used to apped to the
	current menubar/toolbar. The second argument is a list or an
	gmenu or gtoolbar instance.

	The \code{delete} method 
	can be used to delete part of the menubar/toolbar. The
	\code{value} argument can be either a character vector with
	the top-level names to delete, or a named list, or an instance
	of either gmenu or gtoolbar.

	Popular usage reserves toolbars and menubars for top-level
	windows -- not dialog sub windows, or sub groups within a GUI
	-- as such, the container, specified at construction, should
	be a top-level gwindow instance

      }
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
 mbl <- list()
 mbl$File$Open$handler = function(h,...) print("open")
 mbl$File$Quit$handler = function(h,...) print("quit")
 mbl$File$Quit$icon = "quit"
 mbl$Edit$Find$handler = function(h,...) print("Find")
 mbl$Edit$Replace$handler = function(h,...) print("Replace")

 w <- gwindow("gmenu test")
 mb <- gmenu(mbl, container=w)

 tbl <- list()
 tbl$New <- list(icon="new",handler = function(...) print("new"))
 tbl$Print <- list(icon="print",handler = function(...) print("print"))

 tb <- gtoolbar(tbl, container=w)

 ## example of using gaction
 ## works in gWidgetstcltk, but much  better in gWidgetsRGtk2

 ## stub for handler
f <- function(h,...) print("stub")

## some actions. The icon is optional, as is tooltip
aOpen <-  gaction(label="Open",   icon="open",  handler=f)
aClose <- gaction(label="Close",  icon="close", handler=f)
aQuit  <- gaction(label="Quit",   icon="quit",  handler=function(h,...) dispose(w))
aCut <-   gaction(label="Cut",    icon="cut",   handler=f)
aCopy <-  gaction(label="Copy",   icon="copy",  handler=f)
aPaste <- gaction(label="Paste",  icon="paste", handler=f)

## set up groups of actions so that they can be disabled/enabled
## all at once
allActionsGroup <- list(aOpen, aClose, aQuit, aCut, aCopy, aPaste)
editActionsGroup <- list(aCut, aCopy, aPaste)

## define menubar list
ml <- list(File = list(
             open = aOpen,
             close = aClose,
             sep = list(separator = TRUE), # must be named component
             quit = aQuit),
           Edit = list(
             copy = aCopy,
             paste = aPaste))

## toolbar list has only one level
tl <- list(
   Open=aOpen, 
   sep = list(separator = TRUE), # must be named component
   Quit = aQuit)

## set up main window
w <- gwindow()
gmenu(ml, container = w)
gtoolbar(tl, container = w)

## Now add a widget
gbutton(action = aQuit, container = w)

## disable a group of action
sapply(editActionsGroup, function(i) enabled(i) <- FALSE)

}
}
\keyword{interface }% at least one, from doc/KEYWORDS
