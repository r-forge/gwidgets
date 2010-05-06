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
creategwClass <- function(cname) {
  where <- parent.frame()               # what environment to define class


  ## this quiets R CMD
  super <- this <- NULL
  
  ## cname is QWidget
  newClassName <- sprintf("gw%s", cname)
  qtObj <- get(cname, envir=Qt)

  qsetClass(newClassName, qtObj, function(parent=NULL) {
    super(parent)
    this$data <- c()
  },
            where=where)

  ## handler stuff
  ## FUnction call to run a handler
  runHandler <- function(obj, eventName, e, components=character()) {
    handlers <- tag(obj, ".eventHandlers")
    if(is.null(handlers)) 
      return()

    handlers <- handlers[[eventName]]   # list of lists is handlers
    
    h <- list(obj=obj)
    if(length(components))
      for(i in components)
        h[[i]] <- get(i, e)()           # might need to change

    blockedHandlers <- getWithDefault(tag(obj, ".blockedHandlers"), numeric())

    out <- sapply(handlers, function(i) {
      if(i$eventName == eventName) {
        if(!i$id %in% blockedHandlers) {
          h$action <- i$action
          i$handler(h)
        }
      }
    })
    invisible(out)
  }
    

  ## add methods
  newClassObject <- get(newClassName)

  ## method to set the gWidgets Object, should do check
  ## basic flow is 3-step:
  ## e = gwQLineEdit()
  ## obj = new("gEidtQt", block=e, widget=e, ..)
  ## e$setObject(obj)
  qsetMethod("setObject", newClassObject, function(obj) {
    ## obj should be gWidget object -- check
    this$data <- obj
  })
  ## method to get the gWidgets object
  qsetMethod("getObject", newClassObject, function() {
    this$data 
  })

  
  ## set an event handler
  ## called as follows:
  ## getWidget(obj)$setEventHandler("name", handler, action)
  qsetMethod("setEventHandler", newClassObject, function(eventName, handler, action=NULL) {
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

  qsetMethod("removeEventHandler", newClassObject, function(id) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    handlers <- tag(obj, ".eventHandlers")

    for(eventName in names(handlers)) {
      ind <- sapply(handlers[[eventName]], function(i) i$id == id)
      if(length(ind))
        handlers[[eventName]][[which(ind)]] <- NULL
    }
    tag(obj, ".eventHandlers") <- handlers
    invisible()
  })

  qsetMethod("blockEventHandler", newClassObject, function(id) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    handlers <- tag(obj, ".eventHandlers")
    blockedHandlers <- getWithDefault(tag(obj, ".blockedHandlers"), numeric())
    
    if(missing(id)) {
      ## block all
      blockedHandlers <- unlist(sapply(names(handlers), function(eventName) {
        sapply(handlers[[eventName]], function(i) i$id)
      })
                                )
    } else {
      blockedHandlers <- c(id, blockedHandlers)
    }

    tag(obj, ".blockedHandlers") <- blockedHandlers
    invisible()
  })

  qsetMethod("unblockEventHandler", newClassObject, function(id) {
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
  qsetMethod("mousePressEvent", newClassObject, function(e) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    runHandler(obj, "mousePressEvent", e, c("x","y"))
    return(NULL)
  })


  ##' Close Event
  ##' Called when a widget is given close signal from parent
  ##' call event ignore to kill
  ##' We use if the handler returns FALSE we call event ignorm
  ##' Handler must return FALSE to not close window. (Can't be closed if never returns
  qsetMethod("closeEvent", newClassObject, function(e) {
    obj <- this$getObject()
    if(is.null(obj))
      return(NULL)
    out <- runHandler(obj, "closeEvent", e)
    if(is.logical(out) && !out)
      e$ignore()
    else
      e$accept()
    return(NULL)
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


