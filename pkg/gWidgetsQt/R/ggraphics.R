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
##' TODO: Need to add means to close device when window is deleted

setClass("gGraphicsQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

##################################################

##' make a class for a graphics scene so that we can override mouse events, etc.
##' 
qsetClass("OurQGraphicsScene", Qt$QGraphicsScene, function(parent=NULL) {
  super(parent)
  ## properties
  this$lastClick <- NULL
  this$buttonPress <- FALSE
  this$clickHandler <- NULL
  this$changeHandler <- NULL
})



##' add click handler
##' Handler code is simple: Only one handler, use NULL to eliminate. tcltk like. Can make
##' More complicated if need be
qsetMethod("addHandlerClicked", OurQGraphicsScene, function(handler=NULL, action=NULL) {
  this$clickHandler <- handler
  this$clickAction <- action
})

##' changed returns h$x, h$y with x,y coodinates of rectange (lower left, upper right)
qsetMethod("addHandlerChanged", OurQGraphicsScene, function(handler=NULL, action=NULL) {
  this$changeHandler <- handler
  this$changeAction <- action

}) 

##' call the clickHandler if present
qsetMethod("mousePressEvent", OurQGraphicsScene, function(e) {
  this$lastClick <- e$buttonDownScenePos(e$button())
  this$buttonPress <- TRUE
  if(!is.null(clickHandler)) {
    w <- this$width()           
    ht <- this$height()
    x <- this$lastClick$x()
    y <- this$lastClick$y()
    h <- list(obj=this$data,
              x=grconvertX(x/w, from="ndc", to="user"),
              y=grconvertY((ht-y)/ht, from="ndc", to="user"),
              action=this$clickHandlerAction
              )
    clickHandler(h)
  }

  super("mousePressEvent", e)
})


qsetMethod("mouseReleaseEvent", OurQGraphicsScene, function(e) {
  this$lastRelease <- e$scenePos()
  this$buttonPress <- FALSE
   if(!is.null(changeHandler)) {
     w <- this$width()           
     ht <- this$height()
     ## start
     x1 <- this$lastClick$x()
     y1 <- this$lastClick$y()
     ## finish 
     x2 <- this$lastRelease$x()
     y2 <- this$lastRelease$y()
     x <- c(x1,x2); y <- c(y1, y2)
     h <- list(obj=this$data,
               x=sort(grconvertX(x/w, from="ndc", to="user")),
               y=sort(grconvertY((ht-y)/ht, from="ndc", to="user")),
               action=this$changeHandlerAction
               )
     changeHandler(h)
   }

  super("mouseReleaseEvent", e)
})


##################################################

##' make a class for a graphics device. This allows
##' one to write methods for mouse handlers etc.
##' Had trouble using creategwClass, although that would have been preferable
qsetClass("QtDevice", Qt$QGraphicsView)

##' set gWidgets object that this class is the widget of. (self reference)
qsetMethod("setObject", QtDevice, function(obj) this$data <- obj)

##' set as current device
qsetMethod("devSet", QtDevice, function() dev.set(dev))


##' raise on mouse click
##'
qsetMethod("mousePressEvent", QtDevice, function(e) {
  devSet()
  super("mousePressEvent", e)
})


##' only called for top-level windows
##' doesn't work. How to close device when window is destroyed
qsetMethod("closeEvent", QtDevice, function(e) {
  dev.off(dev)
  e$accept()
})

##' initialize the scene for the view
qsetMethod("initScene", QtDevice, function(width, height, pointsize, family="") {
  this$rscene <- qsceneDevice(width, height, pointsize, family, OurQGraphicsScene())
  this$setScene(rscene)

  ## properties
  this$dev <- dev.cur()
  this$clickHandler <- NULL
  this$clickAction <- NULL

  ## setup widget
  setDragMode(Qt$QGraphicsView$RubberBandDrag) # do rubber banding. call addHandlerChanged to get coords
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

            theArgs <- list(...)
            family <- getWithDefault(theArgs$family, "")
            
            v <- QtDevice()
            v$initScene((width/dpi), (height/dpi), ps, family)

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
            v$scene()$addHandlerClicked(handler, action)
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
            v <- getWidget(obj)
            v$scene()$addHandlerChanged(handler, action)
            invisible()
          })
