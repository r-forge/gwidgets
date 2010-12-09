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


## gcheckbox
## uses a button with different shading (YAHOO style)
## methods
## svalue works
## svalue<- works
## names 
## names<-  NO METHOD setBoxLabel

##' checkbox widget
##' 
##' @param text character. text label for checkbox. (Should be that it
##' can be set later with \code{[<-}, but this isn't implemented)
##' @param checked logical. initial state (Set later with \code{svalue<-})
##' @param use.togglebutton logical. If TRUE, represent with a togglebutton, else use check box 
##' @param handler handler called when state is toggled. Check value
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container.
##' @export
##' @note No method to set label (need setBoxLabel)
gcheckbox = function(text, checked = FALSE, use.togglebutton=FALSE,
  handler = NULL, action = NULL,  container = NULL,...) {

  ## dispatch elsewhere if a togglebutton
  if(use.togglebutton) {
    return(gtogglebutton(text, checked, handler,action,container, ...))
  }
  
  widget <- EXTComponentInPanel$new(toplevel=container$toplevel)
  class(widget) <- c("gCheckbox",class(widget))
  widget$setValue(value=checked)
  widget$setValues(value = text)

  ## give a default size, as otherwise panel will spread across screen
  widget$..width <- 200                 # use size()<- to set otherwise
  
  ## define methods

  ## this returns via cat, javascript to set the buttons value
  widget$coerce.with = function(.,x) {
    if(is.character(x)) x <- toupper(x)
    return(as.logical(x))
  }
  widget$getValueJSMethod = "getValue"
  widget$setValueJSMethod = "setValue"
  widget$transportSignal <- "check"   

  ## rather than use   widget$ExtConstructor <- "Ext.form.Checkbox"
  ## we use EXTComponentInPanel and set the xtype here
  widget$ExtCfgOptions <- function(.) {
    list(xtype = "checkbox",
         renderTo = NULL,               # override value
         id = as.character(String(.$ID) + "item"),
         "checked" = svalue(.),
         "boxLabel" = .$getValues()[1]
         )
  }

  ## assign value
  ## we untaint by coercion
  widget$assignValue <- function(., value) {
    svalue(.) <- as.logical(toupper(value[[1]]))
  }
  
  ## Doesn't work
  widget$setValuesJS <- function(., ...) {
    out <- sprintf("%s.boxLabel = '%s';", .$asCharacter(), .$getValues()[1])
    .$addJSQueue(out)
  }

  
  
  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("check",handler=handler,action=action)

  invisible(widget)
}

##' use toggle button to indicate checkbox state
##' 
##' @param text  button text, use [<- to set
##' @param checked value checked or not. Use svalue<- to set
##' @param handler 
##' @param action 
##' @param container 
##' @param ... 
gtogglebutton <- function(text="", checked=TRUE,
                    handler = NULL, action=NULL, container, ...) {
  ## components
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..handler = handler,
                             ..action=action
                             )
  class(widget) <- c("gToggleButton",class(widget))

  widget$setValue(value=checked)
  widget$setValues(value=text)
  

  widget$setValueJS <- function(., ...) {
    out <- String() +
      sprintf("var widget = %s;", .$asCharacter()) +
        sprintf("widget.pressed(%s);", tolower(as.character(.$getValue())))
    .$addJSQueue(out)
  }

  widget$setValuesJS <- function(., ...) {
    out <- sprintf("%s.setText('%s');", .$asCharacter(), .$getValues()[1])
    .$addJSQueue(out)
  }
  
  widget$transportSignal <- "toggle"
  widget$ExtConstructor <- "Ext.Button"
  widget$ExtCfgOptions <- function(.) {
    out <- list("text" = .$getValues()[1],
                "enableToggle"=TRUE,
                "pressed"=svalue(.)
                )
    return(out)
  }


  
  ## add after CSS, scripts defined
  container$add(widget,...)

  if(!is.null(handler))
    widget$addHandlerClicked(handler=handler,action=action)
  
  invisible(widget)
}
  


