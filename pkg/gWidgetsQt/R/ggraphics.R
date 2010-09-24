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

##' ##' Graphics device
##' Uses qtutils work, but places within a class so we have access to mouse events

setClass("gGraphicsQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )


##' make a class for a graphics device. This allows
##' one to write methods for mouse handlers etc.
##' Had trouble using creategwClass, although that would have been preferable
qsetClass("QtDevice", Qt$QGraphicsView)

##' set gWidgets object that this class is the widget of. (self reference)
qsetMethod("setObject", QtDevice, function(obj) this$data <- obj)

##' set as current device
qsetMethod("devSet", QtDevice, function() dev.set(dev))

##' simple handler implementation
##' We can only have one handler at a time. If this is non-NULL, we call it on a mouse click
##' 
qsetMethod("addHandlerClicked", QtDevice, function(handler, action=NULL) {
  this$clickHandler <- handler
  if(!is.null(handler))
    this$clickAction <- action
  else
    this$clickAction <- NULL
})

##' raise on mouse click
##' Beef up -- locator function, rectangle selection
##' XXX The x, y coordinates are not correct (just wrong and don't account for scrollbars). How to fix??
qsetMethod("mousePressEvent", QtDevice, function(e) {
  devSet()
  this$lastClick <- e$pos()
  this$buttonPressed <- TRUE
  if(!is.null(clickHandler)) {
    w <- this$width
    ht <- this$height
    h <- list(obj=this$data,
              x=grconvertX((e$x())/( w), from="ndc", to="user"),
              y=grconvertY((ht-e$y())/ht, from="ndc", to="user"),
              w=w, h=ht, ex=e$x(), ey=e$y()
              )
    clickHandler(h)
  }
  super("mousePressEvent", e)
})

## This didn't work. Try to mimic work in qtdevice to get rubber band selection drawn 
## qsetMethod("mouseMoveEvent", QtDevice, function(e) {
##   this$currentPosition <- e$pos()
##   super("mouseMoveEvent", e)
## })

## qsetMethod("mouseReleaseEvent", QtDevice, function(e) {
##   this$lastRelease <- e$pos()
##   if(this$buttonPressed) {
##     this$buttonPressed <- FALSE
##     update()                            # call paintEvent
##   }
##   super("mouseReleaseEvent", e)
## })

## qsetMethod("paintEvent", QtDevice, function(e) {
  
##   if(exists("buttonPressed", this) && !is.null(buttonPressed) && buttonPressed) {
##     p <- Qt$QPainter(this)
##     p$setBrush(Qt$QBrush(Qt$QColor(127, 127, 144, 31)))
##     p$setPen(Qt$QColor(127, 127, 144, 255));
##     p$drawRect(Qt$QRect(lastClick, currentPosition));
##   }
##   super("paintEvent", e)
## })

##' only called for top-level windows
##' doesn't work. How to close device when window is destroyed
qsetMethod("closeEvent", QtDevice, function(e) {
  dev.off(dev)
  e$accept()
})

##' initialize the scene for the view
qsetMethod("initScene", QtDevice, function(width, height, pointsize, family="") {
  this$rscene <- qsceneDevice(width, height, pointsize, family)
  this$setScene(rscene)

  ## properties
  this$dev <- dev.cur()
  this$clickHandler <- NULL
  this$clickAction <- NULL

  ## setup widget
  setDragMode(Qt$QGraphicsView$ScrollHandDrag)
  setContextMenuPolicy(Qt$Qt$ActionsContextMenu)

  addActions()
})

##' add actions to device
qsetMethod("addActions", QtDevice, function() {
  zoominAct <- Qt$QAction("Zoom In", this)
  zoominAct$setShortcut(Qt$QKeySequence("Ctrl++"))
  qconnect(zoominAct, signal = "triggered", handler = function(checked) {
    this$scale(1.2, 1.2)
  })
  this$addAction(zoominAct)

  zoomoutAct <- Qt$QAction("Zoom Out", this)
  zoomoutAct$setShortcut(Qt$QKeySequence("Ctrl+-"))
  qconnect(zoomoutAct, signal = "triggered", handler = function(checked) {
    this$scale(1/1.2, 1/1.2)
  })
  this$addAction(zoomoutAct)

  printHandler <- function(full = TRUE) {
    printer <- Qt$QPrinter(Qt$QPrinter$HighResolution)
    rpaper <- getOption("papersize")
    if (is.null(rpaper)) 
      rpaper <- "A4"
    qtpaper <- names(Qt$QPrinter)
        usepaper <- qtpaper[match(tolower(rpaper), tolower(qtpaper))]
    if (is.na(usepaper)) 
      usepaper <- "A4"
    printer$setPageSize(Qt$QPrinter[[usepaper]])
    pd <- Qt$QPrintDialog(printer)
    acceptPrint <- pd$exec()
    if (acceptPrint) {
      painter <- Qt$QPainter()
      painter$begin(printer)
      if (full) 
        rscene$render(painter)
      else this$render(painter)
      painter$end()
    }
    pd$d
  }
  printAct <- Qt$QAction("Print", this)
  printAct$setShortcut(Qt$QKeySequence("Ctrl+P"))
  qconnect(printAct, signal = "triggered", handler = function(checked) {
    printHandler(TRUE)
  })
  this$addAction(printAct)

  printVisibleAct <- Qt$QAction("Print visible", this)
  qconnect(printVisibleAct, signal = "triggered", handler = function(checked) {
    printHandler(FALSE)
  })
  this$addAction(printVisibleAct)
  qtutils:::addImageExportAction(this)

})

##' toolkit constructor for ggraphics widget
setMethod(".ggraphics",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,
                   dpi=75, ps=12,
                   container=NULL,...) {

            force(toolkit)
            
            v <- QtDevice()
            v$initScene((width/dpi), (height/dpi), ps, "")

            obj <- new("gGraphicsQt",
                       block=v, widget=v,
                       toolkit=toolkit,
                       e=new.env(), ID=getNewID()
                       )
            v$setObject(obj)            # for events, ...
            
            ## add to container
            if(!is.null(container))
              add(container, obj, ...)

            
            invisible(obj)


          })


### methods

## raise this device
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
                 function(obj, toolkit, ..., value) {
                   if(is.logical(value) == TRUE) {
                     dev.set(tag(obj,"device"))
                   }
                   return(obj)
                 })

##' save Current Page
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   gwCat("svalue not implemented\n")
                   return(obj)
                 })


## ### handlers ## these are from event framework, not signal
## ## add this expose event for graph
## setMethod(".addhandlerexpose",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             addhandler(obj,"expose-event",handler,action)
##           })


## ## applies a handler to the mouse click. The handler gets extra
## ## argument h$x, h$y passed into it. These are in [0,1] coordinates
## setMethod(".addhandlerclicked",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##           })





## applies a handler to the mouse click. The handler gets extra
## argument h$x, h$y passed into it. These are in [0,1] coordinates
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            v <- getWidget(obj)
            v$addHandlerClicked(handler, action)
            invisible()
          })


##' Changed handler is called after rubber band selection is updated
##'
##' Just click and drag out a rubber band
##' The "h" list has components
##' h$x for the x values in user coordinates
##' h$y for the y values in user coordinates
##' These can be converted as in grconvertX(h$x, from="ndc", to="user")
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
          })
