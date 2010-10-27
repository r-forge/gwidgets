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

setClass("gCheckboxQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## constructor
setMethod(".gcheckbox",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text, checked=FALSE,
                   use.togglebutton=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,...) {
            
            force(toolkit)

            ## do we use a toggle button
            if(use.togglebutton)
              return(gtogglebutton(toolkit, text, checked, handler, action, container, ...))
            
            check <- Qt$QCheckBox()
            
            obj <- new("gCheckboxQt",block=check, widget=check,
              toolkit=toolkit)

            if(missing(text)) text = ""
            obj[1] <- text

            svalue(obj) <- as.logical(checked)

            ## add to container
            if(!is.null(container))
              add(container, obj,...)
            
            if (!is.null(handler)) 
              obj@e$handlerID <- addhandlerchanged(obj, handler, action=action)
  
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            state <- w$checkState()
            if(state == 2)
              return(TRUE)
            else if(state == 0)
              return(FALSE)
            else
              return(NULL)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$setChecked(as.logical(value))

                   return(obj)
                 })

## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            w <- getWidget(x)
            w$text
          })
            
setMethod("[",
          signature(x="gCheckboxQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod("[",
                 signature(x="gCheckboxQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxQt"),
          function(x, toolkit, i, j, ..., value) {
            w <- getWidget(x)
            w$setText(value[1])

            return(x)
          })


## names is alias for [
setMethod(".names",signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxQt"),
          function(x, toolkit) {
            x[1]
          })

## can assigne with names(x) <-x or even names(x)[i] <- "single name"
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x = "gCheckboxQt"),
                 function(x,toolkit, value) {
                   x[1] <- value[1]
                 })
### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })

## must work with state value that gets passed in first
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(state, h, ...) {
              h$state <- state
              handler(h)
            }
            ID <- .addhandler(obj, toolkit, "stateChanged", f, action, ...)
            return(ID)
          })


##################################################
setClass("gToggleButtonQt",
         contains="gCheckboxQt"
         )
gtogglebutton <- function(toolkit,
                          text, checked=FALSE,
                          handler=NULL, action=NULL,
                          container=NULL,...) {


  tb <- Qt$QToolButton()
  tb$setCheckable(TRUE)

  obj <- new("gToggleButtonQt",block=tb, widget=tb,
             toolkit=toolkit)

  svalue(obj) <- checked
  obj[] <- text
  
  add(container, obj, ...)

  if(!is.null(handler)) {
    id <- addHandlerChanged(obj, handler=handler, action=action)
    tag(obj, "handler.id") <- id
  }

  obj
}

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToggleButtonQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            return(w$checked)
          })


## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gToggleButtonQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            w <- getWidget(x)
            w$text
          })
            
setMethod("[",
          signature(x="gToggleButtonQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod("[",
                 signature(x="gToggleButtonQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gToggleButtonQt"),
          function(x, toolkit, i, j, ..., value) {
            w <- getWidget(x)
            value <- value[1]
            ## use icon!!!! if there
            if(!is.null(icon <- getStockIconFromName(value)))
              w$setIcon(icon)
            else
              w$setText(value)
            
            return(x)
          })


## names is alias for [
setMethod(".names",signature(toolkit="guiWidgetsToolkitQt",x="gToggleButtonQt"),
          function(x, toolkit) {
            x[1]
          })

## can assigne with names(x) <-x or even names(x)[i] <- "single name"
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x = "gToggleButtonQt"),
                 function(x,toolkit, value) {
                   x[1] <- value[1]
                 })
### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToggleButtonQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })

## must work with state value that gets passed in first
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToggleButtonQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(state, h, ...) {
              handler(h, ...)
            }
            ID <- .addhandler(obj, toolkit, "toggled", f, action, ...)
            return(ID)
          })
