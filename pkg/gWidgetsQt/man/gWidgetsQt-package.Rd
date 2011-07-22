\name{gWidgetsQt-package}
\alias{gWidgetsQt-package}
\alias{gWidgetsQt}
\docType{package}
\title{
Toolkit implementation of gWidgets for Qt
}
\description{

Port of gWidgets API to Qt. The gWidgets API is an abstract,
lightweight means to interact with GUI toolkits, in this case, the Qt
toolkit. 

}
\details{

Containers:
-----------

gwindow: 

Unrealize handler is working. Overrides dispose method though.
If you use a \code{gwindow} instance as a container and pass in the argument \code{do.dockwidget=TRUE} then a the child will be added within a dock widget on the right side of the parent window. Dock widgets can be "undocked".

ggroup:

Need to implement drag motion raise.  The \code{use.scrollwindow}
option works, but it is not perfect. The width and height of the
ggroup object to be displayed must be set. The defaults are large (500
by 2500), but may not be large enough for your use. If they aren't,
then the child widgets get compressed in an odd way.


gframe:

pos isn't quite right
needs to be drawn better -- style issue?

glayout:

gnotebook:

gexpandgroup:

Rolled my own. using a button to check, should have  trigger icon. Resizing is awful.

gsplitter:

Must size window before using svalue<-

WIDGETS
------

gradio:
remove/block etc need work

glabel: 
need to implement editable


gbutton:
icons -- use gWIdgets stock, but should see about Qt especially for
  standard buttons
shortcuts -- use & in label, then Alt-Key will call clicked
  handler. Eg. gbutton("&Ok") for Alt-O

gedit:
keystroke only covers last letter (not enter, ctrl, ...), Give
character. Need to bind to events, not signals.

gslider;

needs to be integer

gstatusbar:

can add a widget. Pass in index=int to specify where. Default is 1. 0 is
the permanent widget:

sb <- gstatusbar()
gbutton("in statusbar", index=1, cont=sb)

gimage:

Handler reports x and y positions in frame

gtext:

keystroke handler same as for gedit -- just last character

gmenu:

popup menus only for buttons (need to bind to mouse events)

gtable:

gbigtable: uses a different model class (\code{qdataFrame}) to make big tables speedy. Is just for
selection, but has handlers: addHandlerChanged (change in selection), in
addition to clicked and doublclick. The \code{svalue} method returns selection, which we force to be rectangular. The selection can be set through \code{svalue} using a row number or data frame with i,j values.

gtree:
no multiple selection(?)
chosencol must be first

gdf:
implement do subset (issue with combobo
implement addHandlerColumnRightClick



gsvg:
Need to implement handlers: (no signals in widget)
Doesn't like labels, etc:
svg("file.svg");
hist(rnorm(100));
dev.off();
gsvg("file.svg",cont=gwindow())

DIALOGS:
--------

gbasicdialog: 

Added hidden argument do.buttons to visible() call. If FALSE, no buttons appear. To
close dialog, call its dispose method within a handler.


METHODS:
---------------
removeHandler, blockHandler, unblockHandler: working for base widgets. 

addHandlerKeystroke: needs event

addHandlerRightClick: not there

Some widgets have events, most use a signal. Eventually to get drag and
drop, I need to use the event approach for most of them


addHandlerIdle: working

font<-: working

tooltip<-: working

size<-: sets Minimum size of widget


  
}
