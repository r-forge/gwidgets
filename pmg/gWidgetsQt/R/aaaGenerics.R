##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

MSG <- function(...) cat("DEBUG",...,"\n")
missingMsg <- function(x) {
  if(missing(x)) x <- "XXX"
  cat("This method",x,"needs to be written\n")
}


## toolkit class
## register classes here for toolkits
setClass("guiWidgetsToolkitQt",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )




##################################################
## put S3 classes from Qt into S4 classes
## got these from apropos("New") -> try(class(do.call(i,list())))

require(qtbase)
setOldClass("RQtObject")
.oldClass <- c("QWidget", "QObject", "QPaintDevice",
               ## containers
               "QMainWindow",
               "QGroupBox", "QButtonGroup",
               "QFrame",
               "QTabWidget",
               "QSplitter",
               "QDialog",
               ## widgets
               "QLabel", 
               "QVBoxLayout", "QHBoxLayout", "QLayout", "QLayoutItem",
               "QGridLayout",
               "QPushButton", "QAbstractButton",
               "QRadioButton",
               "QIcon",
               "QLineEdit",
               "QCheckBox",
               "QDoubleSpinBox", "QSpinBox", "QAbstractSpinBox",
               "QSlider", "QAbstractSlider",
               "QStatusBar",
               "QPixmap",
               "QScrollArea", "QAbstractScrollArea",
               "QComboBox",
               "QTextEdit",
               "QCalendarWidget",
               "QAction",
               "QTableWidget", "QTableWidgetSelectionRange", "QTableView",
               "QTreeWidget", "QTreeView",
               "QAbstractItemView", "QAbstractScrollArea",
               "QMenu", "QMenuBar", "QMenuPopupQt", 
               "QToolBar",
               "QSvgWidget"
               )
sapply(.oldClass, function(i) setOldClass(i))
sapply(.oldClass, function(i) setIs(i, "RQtObject"))
sapply(.oldClass, function(i) setOldClass(sprintf("R::gWidgetsQt::gw%s",i)))
sapply(.oldClass, function(i) setIs(sprintf("R::gWidgetsQt::gw%s",i), "RQtObject"))



setOldClass("try-error")                # for handling try-errors


## a base class which is virtual


##################################################
## A virtual class to hold either RGTK or these guys

## A virtual class for our newly defined objects
## this one contains the ID for the object.
## this may better be done within the NAMESPACE

n=0;assignInNamespace("n",0,"gWidgetsQt")
getNewID = function() {                 # get new one, incremented
  n = getFromNamespace("n",ns="gWidgetsQt")
  assignInNamespace("n",n+1,ns="gWidgetsQt")
  return(n+1)
}
         


setClass("gWidgetQt",
         representation(
                        ID="numeric",
                        e="environment"
                        ),
         prototype(ID=getNewID(), e= new.env())
         )

setClassUnion("guiWidgetORgWidgetQtORQtObject",
              c("guiWidget","gWidgetQt","RQtObject"))


## subclasses
setClass("gComponentQt",
         representation(
                        block="guiWidgetORgWidgetQtORQtObject",
                        widget="guiWidgetORgWidgetQtORQtObject",
                        toolkit="guiWidgetsToolkit"
                        ),
         contains="gWidgetQt",
         )

setClass("gContainerQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## setClass("gContainerQt",
##          representation(
##                         block="guiWidgetORgWidgetQtORQtObject",
##                         widget="guiWidgetORgWidgetQtORQtObject",
##                         toolkit="guiWidgetsToolkit"
##                         ),
##          contains="gWidgetQt",
##          )

## setClass("gContainerQt",
##          representation(
##                         block="guiWidgetORgWidgetQtORQtObject",
##                         widget="guiWidgetORgWidgetQtORQtObject",
##                         toolkit="guiWidgetsToolkit"
##                    ),
##          contains="gWidgetQt",
##          )






##################################################
### Common methods.    Specific to a class are put into the file for that class

## we have two definitions. For instance, "svalue" and ".svalue". The "svalue" method dispatches on the object to the .svalue method. This allows us to use svalue instead of .svalue when defining the methods/constructors inside this package.


setMethod("svalue",signature(obj="gWidgetQt"),
          function(obj, index=NULL, drop=NULL, ...) {
            .svalue(obj, obj@toolkit, ..., index=index, drop=drop)
          })



## svalue
## need method for character and AsIs
setMethod("svalue",signature(obj="character"),
          function(obj, index=NULL, drop=NULL, ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(obj)
                   )
          })
## method for Any is just a pass through
setMethod("svalue",signature(obj="ANY"),
          function(obj, index=NULL, drop=NULL, ...)  {
            return(obj)
          })


setMethod(".svalue",signature(toolkit = "guiWidgetsToolkitQt", obj="character"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(NA)
                   )
          })

## svalue<- -- objec specific
setReplaceMethod("svalue",signature(obj="gWidgetQt"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj, obj@toolkit, index=index, ...) <- value
            obj
          })

                   
                 
## [
setMethod("[",
          signature(x="gWidgetQt"),
          function(x,i,j,...,drop=TRUE) {
            return(.leftBracket(x, x@toolkit,i,j,...,drop=TRUE))
          })

## [<-
setReplaceMethod("[",signature(x="gWidgetQt"),
          function(x,i,j,...,value) {
            if(missing(i) && missing(j))
              .leftBracket(x, x@toolkit,...) <- value
            else if(missing(j))
              .leftBracket(x, x@toolkit,i,...) <- value
            else 
              .leftBracket(x, x@toolkit,i,j,...) <- value
            return(x)
          })

## size ## return size -- not implemented
setMethod("size",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .size(obj, obj@toolkit,...)
          })

setMethod(".size", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gComponentQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
#            return(c(width=w$width, height=w$height))
            return(c(width=w$size$width(),
                     height=w$size$height()))
          })

setMethod(".size", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
          function(obj, toolkit, ...) {
            w <- getBlock(obj)
#            return(c(width=w$width, height=w$height))
            return(c(width=w$size$width(),
                     height=w$size$height()))
          })


## size<-
setReplaceMethod("size",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .size(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkitQt",obj="gComponentQt"),
                 function(obj, toolkit, ..., value) {
                   ## value is width, or width, height
                   w <- getWidget(obj)
                   width <- value[1]
                   if(!is.null(width))
                     w$setMinimumWidth(width)
                   
                   if(length(value) > 1 && !is.null(value[2]))
                     w$setMinimumHeight(value[2])

                   return(obj)
                 })

setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
                 function(obj, toolkit, ..., value) {
                   ## value is width, or width, height
                   w <- getBlock(obj)
                   width <- value[1]
                   if(!is.null(width))
                     w$setMinimumWidth(width)
                   
                   if(length(value) > 1 && !is.null(value[2]))
                     w$setMinimumHeight(value[2])

                   return(obj)
                 })

## ## XXX Do I need this???
## setReplaceMethod(".size", 
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gComponentQt"),
##                  function(obj, toolkit, ..., value) {

##                    return(obj)
##                  })

## ## this works if container has no children (gwindow) but fails otherwise.
## setReplaceMethod(".size", 
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
##                  function(obj, toolkit, ..., value) {
##                    ## pixels for tkframe etc
##                    width <- value[1]
##                    if(length(value) > 1)
##                      height <- value[2]
##                    else
##                      height <- 0
##                    if(height > 0)
##                      tkconfigure(getWidget(obj), width=width, height=height)
##                    else
##                      tkconfigure(getWidget(obj), width=width)

##                    return(obj)
##                  })



## visible
setMethod("visible",signature(obj="gWidgetQt"),
          function(obj, set=NULL, ...) {
            .visible(obj,obj@toolkit, set=set, ...)
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gComponentQt"),
          function(obj, toolkit, set=TRUE, ...) {
            w <- getWidget(obj)
            w$setHidden(!as.logical(set))
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
          function(obj, toolkit, set=TRUE, ...) {
            w <- getBlock(obj)
            w$setHidden(!as.logical(set))
          })

## visible<-
setReplaceMethod("visible",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .visible(obj, obj@toolkit, ...) <- value
            return(obj)
          })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
                 function(obj, toolkit, ..., value) {
                   w <- getWidget(obj)
                   w$setHidden(!as.logical(value))
                   if(value)
                     w$show()
                   return(obj)
                 })

## containers have layout for widget
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
                 function(obj, toolkit, ..., value) {
                   w <- getBlock(obj)
                   w$setHidden(!as.logical(value))
                   if(value)
                     w$show()
                   return(obj)
                 })

## enabled -- TRUE If state is normal
setMethod("enabled",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .enabled(obj, obj@toolkit,...)
          })
setMethod(".enabled",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            return(w$enabled)
          })

## enabled<-
setReplaceMethod("enabled",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .enabled(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
                 function(obj, toolkit, ..., value) {
                   w <- getWidget(obj)
                   w$setEnabled(as.logical(value))
                   
                   return(obj)
                 })

## focus
setMethod("focus",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .focus(obj, obj@toolkit,...)
          })

setMethod(".focus",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            return(w$focus)
          })

## focus<-
setReplaceMethod("focus",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .focus(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".focus",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ..., value) {
            w <- getWidget(obj)
            w$setFocus(as.logical(value))
            
            return(obj)
          })
                 
## default Widget is initially focused
## defaultWidget
setMethod("defaultWidget",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .defaultWidget(obj, obj@toolkit,...)
          })

setMethod(".defaultWidget",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            focus(obj)
          })

## defaultWidget<-
setReplaceMethod("defaultWidget",signature(obj="gWidgetQt"),
                 function(obj, ..., value) {
                   .defaultWidget(obj, obj@toolkit,...) <- value
                   return(obj)
                 })

setReplaceMethod("defaultWidget",signature(obj="RQtObject"),
          function(obj, ..., value) {
            focus(obj) <- TRUE
            return(obj)
          })


## isExtant: True if widget exists
setMethod("isExtant",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .isExtant(obj, obj@toolkit,...)
          })
setMethod(".isExtant",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            out <- try(ls(w), silent=TRUE)
            !inherits(out, "try-error")
          })


## tooltip<-
setReplaceMethod("tooltip",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .tooltip(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".tooltip",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ..., value) {
            w <- getWidget(obj)
            w$setToolTip(value)
            
            return(obj)
          })


## font
setMethod("font",signature(obj="gWidgetQt"),
          function(obj, ...) {
            w <- getWidget(obj)
            f <- w$font
            out <- c(family=f$family(),
                     weight=f$weight(),
                     style=f$style(),
                     size=f$pixelSize())
            ## this stuff means nothing outside of family
            out
          })

## font<-
setReplaceMethod("font",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            .font(obj, obj@toolkit,...) <- value
            return(obj)
          })
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
                 function(obj, toolkit, ..., value) {
                   .font(obj@widget, toolkit, ...) <- value
                   return(obj)
                 })

setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitQt",obj="RQtObject"),
                 function(obj, toolkit, ..., value) {
                   w <- getWidget(obj)
                   f <- makeQFont(value)
                   w$setFont(f)
                   
                   return(obj)
                   
                 })

## undo/red
setMethod(".undo",signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            if(!is.null(w$undo))
              w$undo()
          })
setMethod(".redo",signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            if(!is.null(w$redo))
              w$redo()
          })



## tag, tag<-
## In RGtk2 we used the getData() and setData() methods. In Qt we use the
## crummy implementation from rJava -- a list which grows without bound

setMethod("tag",signature(obj="gWidgetQt"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE
            .tag(obj, obj@toolkit,i, drop=drop,...)
          })
## dispatch in *this* toolkit, not present in obj
setMethod("tag",signature(obj="RQtObject"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE            
            .tag(obj, guiToolkit("Qt"),i, drop=drop,...)
          })

setMethod(".tag", signature(toolkit="guiWidgetsToolkitQt",obj="guiWidget"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                        
            .tag(obj@widget,toolkit,  i, drop=drop,  ...)
          })
setMethod(".tag", signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                                    

            if(is.null(i))
              return(as.list(obj@e))
            else
              return(obj@e[[i]])
            
          })

## tag <-
setReplaceMethod("tag",signature(obj="gWidgetQt"),
          function(obj, i, replace=TRUE, ..., value) {
            .tag(obj, obj@toolkit,i,replace, ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("tag",signature(obj="RQtObject"),
          function(obj,i, replace=TRUE, ..., value) {
            .tag(obj, guiToolkit("Qt"),i, replace, ...) <- value
            return(obj)
          })

## objects can be in many different flavors: guiWIdget, gWidgetQt, QtObject
setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkitQt",obj="guiWidget"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL
            .tag(obj@widget,toolkit,  i, replace, ...) <- value
            return(obj)
          })

setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL
            

            obj@e[[i]] <- value
            return(obj)

          })

##################################################
## id -- define for "ANY" as well
setMethod("id",signature(obj="gWidgetQt"),
          function(obj, ...) {
            tag(obj,".QtID")
          })
setMethod("id",signature(obj="RQtObject"),
          function(obj, ...) {
            tag(obj, ".QtID", ...)
            return(obj)
          })
setMethod("id",signature(obj="ANY"),
          function(obj, ...) {
            if(!is.null(theID<- attr(obj,"id"))) {
              return(theID)
            } else {
              if(is.character(obj)) {
                return(obj[1])
              } else {
                dps = deparse(substitute(obj))
                attr(obj,"id") <- dps
                return(dps)
              }
            }
          })


setMethod(".id", signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            tag(obj,".QtID", ...)
          })

setMethod(".id", signature(toolkit="guiWidgetsToolkitQt",obj="RQtObject"),
          function(obj, toolkit,  ...) {
            return(tag(obj,".QtID"))
          })


## id<-
setReplaceMethod("id",signature(obj="gWidgetQt"),
          function(obj, ..., value) {
            tag(obj,".QtID", ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("id",signature(obj="RQtObject"),
          function(obj, ..., value) {
            tag(obj, ".QtID", ...) <- value
            return(obj)
          })
setReplaceMethod("id",signature(obj="ANY"),
          function(obj, ..., value) {
            attr(obj,"id") <- value
            return(obj)
          })


## we need a .id to handle dispatch from guiWidgets, otherwise, we use id()
setReplaceMethod(".id", signature(toolkit="guiWidgetsToolkitQt",
                                  obj="gWidgetQt"),
          function(obj, toolkit, ..., value) {
            id(obj, ...) <- value
            return(obj)
          })



## add method is biggie
## we have several levels of classes here guiWidget -- gWidgetRGkt -- QtObject, when
## we get down to that level we can finally add
setMethod("add",signature(obj="gWidgetQt"),
          function(obj, value, ...) {
            .add(obj, obj@toolkit,value,...)
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="guiWidget", value="ANY"),
          function(obj, toolkit, value, ...) {
            gwCat(gettext("Can't add without a value\n"))
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="gWidgetQt", value="try-error"),
          function(obj, toolkit, value, ...) {
            gmessage(paste("Error:",obj))
          })
## pushdonw
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="guiWidget", value="guiWidgetORgWidgetQtORQtObject"),
          function(obj, toolkit, value, ...) {
            .add(obj@widget, toolkit, value, ...)
          })

## for gWindow
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="gContainerQt", value="guiWidget"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, value@widget, ...)
          })

## for gContainer
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gContainerQt",value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            ## add value object to obj
            parent <- getWidget(obj)
            child <- getBlock(value)
            


            theArgs <- list(...)
            expand <- getWithDefault(theArgs$expand, FALSE)
            anchor <- getWithDefault(theArgs$anchor, NULL)
            if(!is.null(anchor))
              expand <- TRUE
            fill <- getWithDefault(theArgs$fill, "both") # x, y
            
            ## add -- depends on object
            if(is(child,"QWidget")) {
              ## adding a widget: look at signature of addWidget
              ## 0 is strecth
              ## alignment is from anchor
              stretch <- 0
              if(as.logical(expand)) {
                ## expand can either use fill, or use anchor
                ## if fill we expand widget
                ## if anchor we expand cavity and anchor widget within
                ## this is tcltk stuff. The default is fill = both

                stretch <- 2L
                if(is.null(anchor)) {
                  if(fill == "x")
                    child$setSizePolicy(Qt$QSizePolicy$Fixed, # Preferred? MinimumExpanding?
                                        Qt$QSizePolicy$Expanding)
                  else if(fill == "y")
                    child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                        Qt$QSizePolicy$Fixed)
                  else                  # default is fill = "both" when no anchor, but expand
                    child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                        Qt$QSizePolicy$Expanding)

                  parent$addWidget(child, stretch)
                } else {
                  ## is anchor a numeric vector with x and y
                  if(is.numeric(anchor) && length(anchor) == 2) {
                    ## This is complicated -- too much?
                    ## we have to layouts and add accordingly
                    sublHor <- Qt$QHBoxLayout()
                    sublVert <- Qt$QVBoxLayout()
                    parent$addLayout(sublHor,2)

                    anchor <- as.integer(anchor)
                    if(anchor[1] > 0)
                      sublHor$addStretch(10)

                    sublHor$addLayout(sublVert,2)

                    
                    if(anchor[2] > 0) {
                      sublVert$addWidget(child)
                      sublVert$addStretch(10)
                    } else if(anchor[2] == 0) {
                      sublVert$addWidget(child)
                    } else if(anchor[2] < 0) {
                      sublVert$addStretch(10)
                      sublVert$addWidget(child)
                    }

                    if(anchor[1] < 0)
                      sublHor$addStretch(10)
                  }
                }
              } else {
                ## no expand, stretch == 0
                parent$addWidget(child, 0L) # no stretch
              }
              child$show()
            } else if(is(child, "QLayout")) {
              ## adding a layout
              parent$addLayout(child)
            }
            
            ## record children, parent
            setParent(value, obj)
            addChild(obj, value)
          })



## addSPring, addSpace
setMethod("addSpring",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .addSpring(obj, obj@toolkit,...)
          })

setMethod(".addSpring",
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
          function(obj, toolkit, ...) {
            lyt <- getWidget(obj)
            if(is(lyt, "QLayout"))
              lyt$addStretch(100L)
          })

setMethod("addSpace",signature(obj="gWidgetQt"),
          function(obj, value, ...) {
            .addSpace(obj,obj@toolkit,value,...)
          })

setMethod(".addSpace",
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt"),
          function(obj, toolkit, value, ...) {
            lyt <- getWidget(obj)
            if(is(lyt, "QLayout"))
              lyt$addSpacing(as.integer(value))
          })

## delete -- get down to two QtObjects
setMethod("delete",signature(obj="gWidgetQt"),
          function(obj, widget, ...) {
            .delete(obj, obj@toolkit,widget,...)
          })

## push down to Qt vs Qt. Can be 9 possiblities!
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt",widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj, toolkit, widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gContainerQt",widget="gWidgetQt"),
          function(obj, toolkit, widget, ...) {
            parent <- getWidget(obj)
            child <- getBlock(widget)

            if(is(child, "QWidget")) {
              child$hide()              # hide first
              parent$removeWidget(child)
              removeParent(widget)
              removeChild(obj, widget)
            } else if(is(child, "QLayout")) {
              XXX("How to remove a layout? traverse?")
            }
          })

## dispose -- delete the parent window, or something else
setMethod("dispose",signature(obj="gWidgetQt"),
          function(obj, ...) {
            .dispose(obj, obj@toolkit,...)
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ...) {
            toplevel <- getTopLevel(obj)
            .dispose(toplevel, toolkit)
          })




## update
setMethod("update",signature(object="gWidgetQt"),
          function(object, ...) {
            .update(object, object@toolkit, ...)
          })

setMethod(".update",
          signature(toolkit="guiWidgetsToolkitQt",object="gComponentQt"),
          function(object, toolkit, ...) {
            ## XXX update implemented in sub classes
          })

##
##
##################################################


##################################################
## handlers. Also in aaaHandlers
##

## In Qt we have signals to listen to and events
.addEventHandler <- function(obj, event, handler, action=NULL, ...) {
  ## XXX(sprintf("don't know how to add handler for %s", event))
}



## basic handler for adding with a signal. Not exported.
setGeneric("addhandler", function(obj, signal, handler, action=NULL, ...)
           standardGeneric("addhandler"))
setMethod("addhandler",signature(obj="guiWidget"),
          function(obj, signal, handler, action=NULL, ...) {
            .addhandler(obj@widget, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="gWidgetQt"),
          function(obj, signal, handler, action=NULL, ...) {
            .addhandler(obj, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="RQtObject"),
          function(obj, signal, handler, action=NULL, ...) {
            .addhandler(obj, guiToolkit("Qt"), signal, handler, action, ...)
          })

## method for dispatch
setGeneric(".addhandler",
           function(obj, toolkit,
                  signal, handler, action=NULL, ...)
           standardGeneric(".addhandler"))


setMethod(".addhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="guiWidget"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addhandler(obj@widget, force(toolkit), signal, handler, action, ...)
          })

setMethod(".addhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addHandler(obj, force(toolkit), signal, handler, action, ...)
          })




## Make upcase for Handler
setGeneric(".addHandler",
           function(obj, toolkit,
                  signal, handler, action=NULL, ...)
           standardGeneric(".addHandler"))


setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="guiWidget"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addhandler(obj@widget, force(toolkit), signal, handler, action, ...)
          })


##
## Main one, need to also do events
setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {
            w <- getWidget(obj)
            theArgs <- list(...)
            h <- list(obj=getWithDefault(theArgs$actualobj, obj),
                      action=action)
            theArgs$actualobj <- NULL
            h <- merge(h, theArgs)
            id <- qconnect(w, signal, handler, user.data=h)
            invisible(id)
          })


## removew handler
## removehandler
setMethod("removehandler", signature("gWidgetQt"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, obj@toolkit, ID, ...)
          })
setMethod("removehandler", signature("RQtObject"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, guiToolkit("Qt"), ID, ...)
          })

## Main one
setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            ## we need to check if this is a timer handler
            timerHandlers <- tag(obj, "timers")
            for(i in timerHandlers) {
              if(digest(i$id) == digest(ID)) {
                i$timer$stop()
                return()
              }
            }
            ## otherwise, we pass off
            
            .removehandler(getWidget(obj),toolkit,ID,...)
          })

setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="RQtObject"),
          function(obj, toolkit, ID=NULL, ...) {
            if(is.null(ID))
              obj$disconnect()
            else
              XXX("Can only remove all handlers. Call without ID.")
          })


## blockhandler
setMethod("blockhandler", signature("gWidgetQt"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj, obj@toolkit, ID, ...)
          })
setMethod("blockhandler", signature("RQtObject"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj, guiToolkit("Qt"), ID, ...)
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            w <- getWidget(obj)
            w$blockSignals(TRUE)
            if(!is.null(ID))
              XXX("can only block all signals, so we did.")
          })

## setMethod(".blockhandler",
##           signature(toolkit="guiWidgetsToolkitQt",obj="RQtObject"),
##           function(obj, toolkit, ID=NULL, ...) {
##           })



## unblock handler
setMethod("unblockhandler", signature("gWidgetQt"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(obj, obj@toolkit, ID, ...)
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            w <- getWidget(obj)
            w$blockSignals(FALSE)
            if(!is.null(ID))
              XXX("Can only unblock all signals, so did")
          })




## addhandlerchanged
setMethod("addhandlerchanged",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, obj@toolkit, handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, guiToolkit("Qt"), handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="ANY"),
          function(obj, handler=NULL, action=NULL, ...) {
            warning("No method addhandlerchanged for object of class",class(obj),"\n")
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="clicked",
                        handler=handler, action=action, ...)
          })


## expose: expose-event or realize
setMethod("addhandlerexpose",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerexpose",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj, guiToolkit("Qt"), handler, action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="mapped",
                        handler=handler, action=action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gComponentQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="realize",
                        handler=handler, action=action, ...)
          })

## unrealize: unrealize
setMethod("addhandlerunrealize",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerunrealize",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="unmap",
                        handler=handler, action=action, ...)
          })

## destroy: destroy
setMethod("addhandlerdestroy",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdestroy",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="destroyed",
                        handler=handler, action=action, ...)
          })

## keystroke: changed
setMethod("addhandlerkeystroke",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerkeystroke",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            ## XXX no default signal, see gedit, gtext
          })


## clicked: clicked
setMethod("addhandlerclicked",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerclicked",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="clicked",
                        handler=handler, action=action, ...)
          })

## doubleclick: no default
setMethod("addhandlerdoubleclick",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdoubleclick",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="clicked",
                        handler=handler, action=action, ...)
          })

## rightclick: button-press-event -- handle separately
setMethod("addhandlerrightclick",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerrightclick",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerrightclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            ## XXX("right click event???")
          })

## column click things
setMethod("addhandlercolumnclicked",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlercolumnclicked(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlercolumnrightclick",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlercolumnrightclick(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlercolumndoubleclick",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlercolumndoubleclick(obj, obj@toolkit,handler, action, ...)
          })



## focus
setMethod("addhandlerfocus",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerfocus(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerfocus",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerfocus(obj,guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerfocus",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="focus",
                        handler=handler, action=action, ...)
          })
## blur
setMethod("addhandlerblur",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerblur(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerblur",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerblur(obj,guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlerblur",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            ### XXX("An event handler is needed")
            return(NULL)
            .addEventHandler(obj, "focusOutEvent",
                             handler=handler, action=action, ...)
          })



## mousemotion
setMethod("addhandlermousemotion",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlermousemotion(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlermousemotion",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlermousemotion(obj,guiToolkit("Qt"),handler, action, ...)
          })

setMethod(".addhandlermousemotion",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="XXX",
                        handler=handler, action=action, ...)
          })

## idle
setMethod("addhandleridle",signature(obj="gWidgetQt"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, obj@toolkit,
                            handler=handler, action=action, interval=interval, ...)
          })
setMethod("addhandleridle",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, guiToolkit("Qt"),
                            handler=handler, action=action, interval=interval, ...)
          })

setMethod(".addhandleridle",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit,
                   handler=NULL, action=NULL, interval=1000, ...) {
            timer <- Qt$QTimer()
            h <- list(obj=obj, action=action)
            id <- qconnect(timer,"timeout",handler, user.data=h)
            timers <- tag(obj, "timers")
            if(is.null(timers))
              timers <- list()
            timers[[length(timers) + 1]] <- list(id=id, timer=timer)
            tag(obj,"timers") <- timers
            timer$start(as.integer(interval))

            invisible(id)
          })

## popupmenu
setMethod("addpopupmenu",signature(obj="gWidgetQt"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, obj@toolkit, menulist, action, ...)
          })
### XXX This uses context menu for popup -- may not be first mouse button
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            m <- .gmenu(toolkit, menulist, popup=TRUE)
            add(obj, m)
          })
setMethod("addpopupmenu",signature(obj="RQtObject"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, guiToolkit("Qt"), menulist, action, ...)
          })




## add3rdmousepopupmenu
setMethod("add3rdmousepopupmenu",signature(obj="gWidgetQt"),
          function(obj, menulist, action=NULL, ...) {
            .add3rdmousepopupmenu(obj, obj@toolkit, menulist, action, ...)
          })
setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWidgetQt"),
          function(obj, toolkit, menulist,action=NULL, ...) {
            m <- .gmenu(toolkit, menulist, popup=TRUE)
            add(obj, m)
          })

setMethod("add3rdmousepopupmenu",signature(obj="RQtObject"),
          function(obj, menulist, action=NULL,...) {
            .add3rdmousepopupmenu(obj, guiToolkit("Qt"),menulist, action,...)
          })


## "dotmethods" defined in dnd.R
## adddropsource
setMethod("adddropsource",signature(obj="gWidgetQt"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })
setMethod("adddropsource",signature(obj="RQtObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, guiToolkit("Qt"),targetType=targetType,
                           handler=handler, action=action, ...)
          })

## adddropmotion
setMethod("adddropmotion",signature(obj="gWidgetQt"),
          function(obj,  handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, obj@toolkit,
                           handler=handler, action=action, ...)
          })
setMethod("adddropmotion",signature(obj="RQtObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, guiToolkit("Qt"),
                           handler=handler, action=action, ...)
          })

## adddroptarget
setMethod("adddroptarget",signature(obj="gWidgetQt"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })

setMethod("adddroptarget",signature(obj="RQtObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, guiToolkit("Qt"),targetType=targetType,
                           handler=handler, action=action, ...)
          })


## R Methods
setMethod("dim", "gWidgetQt", function(x) .dim(x,x@toolkit))
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkitQt",x="gWidgetQt"),
          function(x,toolkit) {
            gwCat(sprintf("Define dim for x of class: %s", class(x)[1]))
            return(NULL)
})
setMethod("length", "gWidgetQt", function(x) .length(x,x@toolkit))
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gWidgetQt"),
          function(x,toolkit) {
            gwCat(sprintf("Define length for x of class:%s\n"),class(x)[1])
            return(NULL)            
})
          
setMethod("dimnames", "gWidgetQt", function(x) .dimnames(x,x@toolkit))
setReplaceMethod("dimnames",
                 signature(x="gWidgetQt"),
                 function(x,value) {
                   .dimnames(x,x@toolkit) <- value
                   return(x)
                 })

setMethod("names", "gWidgetQt", function(x) .names(x,x@toolkit))
setReplaceMethod("names",
                 signature(x="gWidgetQt"),
                 function(x,value) {
                   .names(x,x@toolkit) <- value
                   return(x)
                 })
