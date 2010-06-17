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

##' In order to get addHandlerUnrealize I need to use the event framework, but
##' that is *really* flaky. I believe the issue is not being able to call deleteLater to
##' remove/close a window. Trying $close() and $~QtMainWindow cause the windows to be much
##' more unstable -- unable to open/close repeatedly.
##' To put back the event widget stuff, just uncomment ##XX comments.

setClass("gWindowQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
##XX         contains="gEventWidgetQt",
##XX         prototype=prototype(new("gEventWidgetQt"))
         )

## Qt constructor
##XX creategwClass("QMainWindow")

## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL, parent=NULL,
                   handler=NULL, action = NULL,
                   ...
                   ) {

            force(toolkit)

            ## make object

            if(!is.null(parent)) {
              toplevel <- getTopLevel(parent)
              parentw <- getBlock(toplevel)
              w <- Qt$QMainWindow(parentw)
##XX              w <- gwQMainWindow(parentw)
            } else {
              w <- Qt$QMainWindow()
##XX              w <- gwQMainWindow()
            }

            ## main widget
            w1 <- Qt$QWidget()
            w$setCentralWidget(w1)
            lyt <- Qt$QVBoxLayout()
            w1$setLayout(lyt)

            
            obj <- new("gWindowQt", block=w, widget=lyt,
                       toolkit=toolkit,e=new.env(), ID=getNewID()
                       )
##XX            w$setObject(obj)

            svalue(obj) <- title
            size(obj) <- c(width=width, height=height)
            
            if (!is.null(handler)) {
              tag(obj, "handler.id") <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            visible(obj) <- as.logical(visible)
            
            return(obj)
          })
##################################################
## Methods 
## getToolkitWidget returns window -- not frame

## return window -- not frame
setMethod(".getToolkitWidget",
          signature(obj="gWindowQt", toolkit="guiWidgetsToolkitQt"),
          function(obj, toolkit) obj@block)



## general add
## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", value="gWidgetQt"),
##           function(obj, toolkit, value, ...) {
##             ## us there anything different here?
##           })


## add toolbar, menubar, statusbar
## XXX menubar -- in gmenu
## toolbar
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", value="gMenuQt"),
          function(obj, toolkit, value, ...) {
            w <- getBlock(obj)
            mb <- getBlock(value)
            w$setMenuBar(mb)
          })

## toolbar
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", value="gToolbarQt"),
          function(obj, toolkit, value, ...) {
            w <- getBlock(obj)
            tb <- getBlock(value)
            w$addToolBar(tb)
          })

## statusbar
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", value="gStatusbarQt"),
          function(obj, toolkit, value, ...) {
            w <- getBlock(obj)
            sb <- getBlock(value)
            w$setStatusBar(sb)
          })


## methods

##' svalue refers to title
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            w <- getBlock(obj)
            w$windowTitle
          })
##' svalue<_ to set title
##' all arguments are ignored
setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, index=NULL,..., value) {
            w <- getBlock(obj)
            w$windowTitle <- as.character(value)
            return(obj)
          })

##' change size. The size() method is inherited from gContainerQt
setReplaceMethod(".size",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
                 function(obj, toolkit, ..., value) {
                   w <- getBlock(obj)
                   value <- as.integer(value)
                   if(length(value) >= 2)
                     w$resize(value[1], value[2])
                   
                   return(obj)
           })

##" Close the window
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, ...) {
            w <- getBlock(obj)
            ## Really need to do deleteLater -- but that is a slot
            ##get("~QMainWindow", envir=w)()
            w$close()
            return()
          })

##' show/hide the window
setReplaceMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, ...,value) {
            w <- getBlock(obj)
            if(as.logical(value))
              w$show()
            else
              w$hide()
            if(is.list(tag(obj, "children")))
              sapply(tag(obj, "children"), function(i) visible(i) <- value)

            return(obj)
            })
          

##################################################
## handlers

setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit,  handler, action=NULL, ...) {
            w <- getBlock(obj)
            cat("Unable to implement\n")
          })

##XX if eventwidget stuff works, then this can be implemented:
##XX setMethod(".addhandlerunrealize",
##XX           signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
##XX           function(obj, toolkit,  handler, action=NULL, ...) {
##XX             w <- getBlock(obj)
##XX             id <- w$setEventHandler("closeEvent", handler, action)
##XX             invisible(id)
##XX           })


## setMethod(".addhandlerdestroy",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             .addHandler(obj, toolkit, signal="destroy", handler, action, ...)
##           })