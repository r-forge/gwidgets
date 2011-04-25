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

## extend the base class so that we can set the event handlers
## This creates a class gwXXX where XXX is the original Qt$XXX

## This is going to be much slower than using a corresponding signal, it
## it exists. In qtbase, events are handled at the class level, not the instance
## level, so we create a subclass to do our processing

creategwClass <- function(cname, constructor) {
  where <- parent.frame()               # what environment to define class


  ## this quiets R CMD
  super <- this <- NULL
  
  ## cname is QWidget
  newClassName <- sprintf("gw%s", cname)
  qtObj <- get(cname, envir=Qt)

  if(missing(constructor)) {
    constructor <- function(parent=NULL) {
      super(parent)
      this$data <- c()
      
      this$dndStartPosition <- NULL
      this$dropHandler <- NULL            # function(obj, data) data is passed
      this$dragHandler <- NULL            # function(obj, e) e is mouse event. Returns value to pass
    }
  }
  
  qsetClass(newClassName, qtObj, constructor, where=where)


  ## add methods
  NewClassObject <- get(newClassName)

  ## handler stuff
  ##' FUnction call to run a handler
  ##' @param obj gWidgets object
  ##' @param eventName name of event
  ##' @param e mouse event passed in by Qt
  ##' @param components
  gwRunQtEventHandler <- function(obj, eventName, e, components=character()) {
    handlers <- tag(obj, ".eventHandlers")
    if(is.null(handlers)) 
      return()
    
    handlers <- handlers[[eventName]]   # list of lists is handlers
    blockedHandlers <- getWithDefault(tag(obj, ".blockedHandlers"), numeric())

    h <- list(obj=obj)
    if(length(components))
      for(i in components)
        h[[i]] <- get(i, e)()           # might need to change
    
    out <- lapply(handlers, function(i) {
      if(i$eventName == eventName) {
        if(length(blockedHandlers) == 0 || !i$id %in% blockedHandlers) {
          h$action <- i$action
          i$handler(h)
        }
      }
    })
    invisible(out)
  }
    


  ## method to set the gWidgets Object, should do check
  ## basic flow is 3-step:
  ## e = gwQLineEdit()
  ## obj = new("gEidtQt", block=e, widget=e, ..)
  ## e$setObject(obj)
  qsetMethod("setObject", NewClassObject, function(obj) {
    ## obj should be gWidget object -- check
    this$data <- obj
  })

  ## method to get the gWidgets object
  qsetMethod("getObject", NewClassObject, function() {
    this$data 
  })

  ## set an event handler
  ## called as follows:
  ## getWidget(obj)$setEventHandler("name", handler, action)
  qsetMethod("setEventHandler", NewClassObject, function(eventName, handler, action=NULL) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    n <- getWithDefault(tag(obj, ".nEventHandlers"), 0)
    id <- n+1
    tag(obj, ".nEventHandlers") <- id
    
    l <- list(eventName=eventName,
              handler=handler,
              action=action,
              id=id)
    
    handlers <- tag(obj, ".eventHandlers")
    if(is.null(handlers))
      handlers <- list()
    if(is.null(handlers[[eventName]]))
      handlers[[eventName]] <- list()
    handlers[[eventName]][[id]] <- l
    tag(obj, ".eventHandlers") <- handlers

    class(id) <- c("gWidgetsQtEventHandler", class(id))
    return(id)
  })

  qsetMethod("removeEventHandler", NewClassObject, function(id) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    handlers <- tag(obj, ".eventHandlers")

    for(eventName in names(handlers)) {
      ind <- sapply(handlers[[eventName]], function(i) i$id == id)
      if(length(unlist(ind)))
        handlers[[eventName]][[which(ind)]] <- NULL
    }
    tag(obj, ".eventHandlers") <- handlers
    invisible()
  })

  ##' block an event handler
  ##' @param id id from adding event handler
  qsetMethod("blockEventHandler", NewClassObject, function(id) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    handlers <- tag(obj, ".eventHandlers")
    blockedHandlers <- getWithDefault(tag(obj, ".blockedHandlers"), numeric())
    
    if(missing(id)) {
      ## block all
      blockedHandlers <- unlist(lapply(names(handlers), function(eventName) {
        sapply(handlers[[eventName]], function(i) i$id)
      })
                                )
    } else {
      blockedHandlers <- c(id, blockedHandlers)
    }

    tag(obj, ".blockedHandlers") <- blockedHandlers
    invisible()
  })

  ##' unblock an event handler
  ##' @param id id from addEventHandler
  qsetMethod("unblockEventHandler", NewClassObject, function(id) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    handlers <- tag(obj, ".eventHandlers")
    blockedHandlers <- getWithDefault(tag(obj, ".blockedHandlers"), numeric())

    if(missing(id)) {
      ## unblock all
      blockedHandlers <- numeric()
    } else {
      blockedHandlers <- setdiff(blockedHandlers, id)
    }

    tag(obj, ".blockedHandlers") <- blockedHandlers
    invisible()
  })


  ## The are various events that we have implemented
  ## we can pass in event values, see x,y below
  qsetMethod("mousePressEvent", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      this$dndStartPosition <- e$pos()    # for drag and drop
    }
    
    obj <- this$getObject()
    if(!is.null(obj)) {
      gwRunQtEventHandler(obj, "mousePressEvent", e, c("x","y"))
    }

    super("mousePressEvent", e)
  })

  ##' set a function for handling a drag event
  qsetMethod("setDragHandler", NewClassObject, function(f) {
    this$dragHandler <- f               # function(obj, e)
  })

  ##' set up drag and drop event if present
  qsetMethod("mouseMoveEvent", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      if ((e$buttons() & Qt$Qt$LeftButton) && !is.null(this$dndStartPosition)) {
        dist <- (e$x() - this$dndStartPosition$x())^2 +  (e$y() - this$dndStartPosition$y())^2
        if (dist >= Qt$QApplication$startDragDistance()^2)
          this$prepareDrag(e)
      }
    }
    super("mouseMoveEvent", e)
  })

  
  ##' prepare drag event. Requires dragHandler
  qsetMethod("prepareDrag", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      val <- this$dragHandler(this$getObject(), e)
      md <- Qt$QMimeData()
      md$setData("R/serialized-data", serialize(val, NULL))

      drag <- Qt$QDrag(this)
      drag$setMimeData(md)
  
      drag$exec()
    }
  })

  ##' when we enter we change the color palette
  qsetMethod("dragEnterEvent", NewClassObject, function(e) {
    if(!is.null(this$dropHandler)) {
      this$setForegroundRole(Qt$QPalette$Dark)
      e$acceptProposedAction()
    }

    super("dragEnterEvent", e)
  })
  
  ##' when we leave we return the palette
  qsetMethod("dragLeaveEvent", NewClassObject, function(e) {
    if(is.null(this$dropHandler)) return()
    this$setForegroundRole(Qt$QPalette$WindowText)
    e$accept()

    super("dragLeaveEvent", e)    
  })

  ##' drop event calls dropHandler (set via setDropHandler)
  qsetMethod("dropEvent", NewClassObject, function(e) {
    if(!is.null(this$dropHandler)) {
      this$setForegroundRole(Qt$QPalette$WindowText)  
      md <- e$mimeData()
      if(md$hasFormat("R/serialized-data")) {
        data <- unserialize(md$data("R/serialized-data"))
        this$dropHandler(this$getObject(), data)
        this$setBackgroundRole(Qt$QPalette$Window)
        e$acceptProposedAction()
      }
    }

    super("dropEvent", e)
  })
  
  ##' set a dropHandler. This implements drop area
  ##' f <- function(obj, value)
  qsetMethod("setDropHandler", NewClassObject, function(f) {
    this$setAcceptDrops(TRUE)
    this$dropHandler <- f
  })

  ##' Close Event
  ##' Called when a widget is given close signal from parent
  ##' call event ignore to kill
  ##' We use if the handler returns FALSE we call event ignorm
  ##' Handler must return FALSE to not close window. (Can't be closed if never returns
  qsetMethod("closeEvent", NewClassObject, function(e) {
    obj <- this$getObject()
    if(!is.null(obj)) {
      out <- gwRunQtEventHandler(obj, "closeEvent", e)
      if(is.logical(out) && !out)
        e$ignore()
      else
        e$accept()
    }
    super("closeEvent", e)
  })

  ##' Focus events
  qsetMethod("focusInEvent", NewClassObject, function(e) {

      obj <- this$getObject()
      if(!is.null(obj)) {
        gwRunQtEventHandler(obj, "focusInEvent", e)
      }

    super("focusInEvent", e)
  })
} 




##################################################
## gWidgets methods for events
### Handler, use events here
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit,  handler, action=NULL, ...) {
            w <- getWidget(obj)
            id <- w$setEventHandler("mousePressEvent", handler, action)
            invisible(id)
          })

setMethod(".addhandlerfocus",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit,  handler, action=NULL, ...) {
            w <- getWidget(obj)
            id <- w$setEventHandler("focusInEvent", handler, action)
            invisible(id)
          })

##' method can return logical. If FALSE, then close does not occure
setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit,  handler, action=NULL, ...) {
            w <- getWidget(obj)
            id <- w$setEventHandler("closeEvent", handler, action)
            invisible(id)
          })



setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            w <- getWidget(obj)
            if(is.null(ID))
              w$removeEventHandler()
            else
              w$removeEventHandler(ID)
          })
setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            w <- getWidget(obj)
            if(is.null(ID))
              w$blockEventHandler()
            else
              w$blockEventHandler(ID)
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEventWidgetQt"),
          function(obj, toolkit, ID=NULL, ...) {
            w <- getWidget(obj)
            if(is.null(ID))
              w$unblockEventHandler()
            else
              w$unblockEventHandler(ID)
          })


