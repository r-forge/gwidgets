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
#         contains="gContainerQt",
#         prototype=prototype(new("gContainerQt"))
         representation = representation("gEventWidgetQt",
           centralWidget="QWidget"),
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## Qt constructor
creategwClass("QMainWindow")

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


            ##' helper to set x,y position
            setPos <- function(w, pos) {
              w$move(pos[1], pos[2])
            }
            
            ## make window object w
            if(!is.null(parent)) {
              if(is.numeric(parent)) {
                ## specifies x,y coordinates
                w <- gwQMainWindow()
                w$move(parent[1], parent[2])
              } else {
                parentw <- getBlock(parent)
                g <- parentw$geometry
                pos <- c(g$x(), g$y()) + 20 # offset
                w <- gwQMainWindow()
                w$move(pos[1], pos[2])
                
                ## make transient ## isn't working as w does not get destroyed event
                ## http://stackoverflow.com/questions/4456252/qt-multiple-windows-in-a-parent-child-chain-parent-does-not-close-children ????
                addHandlerDestroy(parent, handler=function(h,...) h$action$close(), action=w)
#                qconnect(parentw, "closeEvent", function(...) w$close())
              }
            } else {
#              w <- Qt$QMainWindow()
              w <- gwQMainWindow()
            }

            ## main widget
            w1 <- Qt$QWidget()
 #           w1$setSizePolicy(Qt$QSizePolicy$Expanding,
 #                            Qt$QSizePolicy$Expanding)
            
            w$setCentralWidget(w1)
            lyt <- Qt$QVBoxLayout()
            w1$setLayout(lyt)

            
            obj <- new("gWindowQt", block=w, widget=lyt,
                       centralWidget=w1,
                       toolkit=toolkit,e=new.env(), ID=getNewID()
                       )
            w$setObject(obj)

            svalue(obj) <- title
            if(!is.null(width))
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
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            theArgs <- list(...)
            if(!is.null(theArgs$do.dockwidget)) {
              w <- getBlock(obj)
              child <- getBlock(value)
              dw <- Qt$QDockWidget()
              dw$setWidget(child)
              w$addDockWidget(Qt$Qt$RightDockWidgetArea, dw)
            } else {
              ## do this add ourselves
              lyt <- getWidget(obj)
              child <- getBlock(value)
              expand <- getWithDefault(theArgs$expand, TRUE)
              if(expand)
                child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                    Qt$QSizePolicy$Expanding)

              anchor <- getWithDefault(theArgs$anchor, NULL)
              lyt$addWidget(child, stretch=1, xyToAlign(anchor))
            }
          })


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
            visible(obj) <- TRUE
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

##' return size of main window
setMethod(".size",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, ...) {
            w <- getBlock(obj)
            c(width=w$size$width(), height=w$size$height())
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
            ## XXX Really need to do deleteLater -- but that is a slot
            ##get("~QMainWindow", envir=w)()
            w$close()
            w$setParent(NULL)
            return()
          })

##' show/hide the window
setReplaceMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, ...,value) {
            w <- getBlock(obj)
            if(as.logical(value)) {
              w$show()
              w$raise()                 # top of stack
              w$activateWindow()        # keyfocus
            } else {
              w$hide()
            }
            if(is.list(tag(obj, "children")))
              lapply(tag(obj, "children"), function(i) visible(i) <- value)

            return(obj)
            })
          
##' Recompute size
setMethod(".update",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(object, toolkit, ...) {
            ## XXX implement me
            invisible()
          })



##' focus will raise window
##'
##' @param object gwindow object
##' @param toolkit name of toolkit
##' @param ... ignored
##' @return NULL called for side effect of raising window
setMethod(".focus",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, ...) {
            w <- getBlock(obj)
            w$raise()                 # top of stack
            w$activateWindow()        # keyfocus
          })

setReplaceMethod(".focus",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
                 function(obj, toolkit, ..., value) {
                   if(as.logical(value)) {
                     w <- getBlock(obj)
                     w$raise()                 # top of stack
                     w$activateWindow()        # keyfocus
                   }
                   return(obj)
                 })



## delete
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt",widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj, toolkit, widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt", widget="gComponentQt"),
          function(obj, toolkit, widget, ...) {
            ## remove from central widget
            parent <- obj@centralWidget$layout()
            child <- getBlock(widget)
            ## we don't have removeParent, removeChild as we don't do accounting here
            child$hide()              # hide first
            parent$removeWidget(child)
          })


##################################################
## handlers

## setMethod(".addhandlerunrealize",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
##           function(obj, toolkit,  handler, action=NULL, ...) {
##             w <- getBlock(obj)
##             cat("Unable to implement\n")
##           })

### if eventwidget stuff works, then this can be implemented:
setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit,  handler, action=NULL, ...) {
            w <- getBlock(obj)
            id <- w$setEventHandler("closeEvent", handler, action)
            invisible(id)
          })


setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitQt",obj="gWindowQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerunrealize(obj, toolkit, handler, action, ...)
#            .addHandler(obj, toolkit, signal="destroy", handler, action, ...)
          })
