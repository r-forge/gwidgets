\name{as.gWidgetsRGtk2}
\alias{as.gWidgetsRGtk2}
\alias{as.gWidgetsRGtk2.GtkHBox}
\alias{as.gWidgetsRGtk2.GtkVBox}
\alias{as.gWidgetsRGtk2.GtkImage}
\alias{as.gWidgetsRGtk2.GtkLabel}
\alias{as.gWidgetsRGtk2.GtkTable}
\alias{as.gWidgetsRGtk2.GtkNotebook}
\alias{as.gWidgetsRGtk2.GtkHPaned}
\alias{as.gWidgetsRGtk2.GtkVPaned}
\alias{as.gWidgetsRGtk2.GtkRadioButton}
\alias{as.gWidgetsRGtk2.GtkHSeparator}
\alias{as.gWidgetsRGtk2.GtkVSeparator}
\alias{as.gWidgetsRGtk2.GtkHScale}
\alias{as.gWidgetsRGtk2.GtkVScale}
\alias{as.gWidgetsRGtk2.GtkSpinButton}
\alias{as.gWidgetsRGtk2.GtkStatusbar}
\alias{as.gWidgetsRGtk2.GtkTextView}
\title{Coerce an RGtk2 object into a gWidgetsRGtk2 object}
\description{
  This function coerces an RGtk2 object into a gWidgetsRGtk2 object,
  thereby allowing most of the methods to work on the new object.
}
\usage{
as.gWidgetsRGtk2(widget, ...)
}
\arguments{
  \item{widget}{An object of class \code{RGtkObject}}
  \item{\dots}{ Ignored here }
}
\details{
  Many RGtk2 widgets can be coerced into gWidgetsRGtk2 objects. This
  allows the method of gWidgets to be called. The example shows how one
  can use glade to layout a dialog, and use gWidget methods for the handlers.
}
\value{
  Returns a \code{gWidgetsRGtk2} object. (This is not a \code{gWidgets}
  object, so there may be some oddities
}
\examples{
\dontrun{
## This requires glade libraries to be installed before compiling RGtk2
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets)
library(gWidgetsRGtk2)

gladeFile <- system.file("examples/t.test.glade",package="gWidgetsRGtk2")
GUI <- gladeXMLNew("t.test.glade")

w <- GUI$GetWidget("window1")
w$Show()                                # show
win <- as.gWidgetsRGtk2(w)

gladeXMLGetWidgetNames <- function(obj) {
  sapply(obj$GetWidgetPrefix(""),gladeGetWidgetName)
}

gladeXMLGetgWidgetsRGtk2 <- function(obj) {
  nms <- obj$GetWidgetNames()
  widgets <- sapply(nms, function(i) obj$GetWidget(i))
  widgets <- sapply(widgets, as.gWidgetsRGtk2)
  return(widgets)
}

l <- GUI$GetgWidgetsRGtk2()

## val names have similar form
valNames <- grep("Val$",GUI$GetWidgetNames())
defHandler <- function(...) {
  lst <- list()
  args <- c("x","y", "mu","alt","var.equal","paired","conf.level")
  for(i in args) {
    key <- paste(i,"Val",sep="")
    widget <- l[[key]]
    val <- svalue(widget)
    if(!is.null(val) && val != "")
      lst[[i]] <- val
  }
  if(!is.null(lst$x)) {
    cmd <- "t.test("
    argList <- c()
    for(i in names(lst)) {
      argList <- c(argList,paste(i,"=",lst[[i]], sep=""))
    }
    cmd <- paste(cmd, paste(argList,collapse=", "),")",sep="")
    print(cmd)
  }      
}  
## Add handler to each widget
sapply(valNames, function(i) addHandlerChanged(l[[i]],handler=defHandler))
## put handler on dismiss button
addHandlerChanged(l[['dismiss']], handler = function(h,...) dispose(win))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}
