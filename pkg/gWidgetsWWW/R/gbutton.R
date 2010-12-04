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


## button -
## methods
## svalue works
## svalue<- works
## addHandlerClicked works

## *IF* handler = NULL and action a gaction instance then
## will use that action. Not for addHandlerClicked though.

##' button widget
##'
##' 
##' @param text button text. See \code{svalue} to change
##' @param border logical. If \code{FALSE} will not draw border
##' @param handler click handler
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @export
gbutton <- function(text="", border=TRUE,
                    handler = NULL, action=NULL, container, ...) {
  ## components
  widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                             ..handler = handler,
                             ..action=action
                             )
  class(widget) <- c("gButton",class(widget))
  widget$setValue(value=text)

  ## function to check is we have a gaction object
  widget$doAction <- function(.) {
    if(!exists("..handler", envir=., inherits=FALSE) &&
       exists("..action", envir=., inherits=FALSE) &&
       !is.null(.$..action) &&
       inherits(.$..action,"gAction")
       ) return(TRUE)
    if(is.null(.$..handler) &&
       exists("..action", envir=., inherits=FALSE) &&
       !is.null(.$..action) &&
       inherits(.$..action,"gAction")
       ) return(TRUE)
    return(FALSE)
  }
  ## properties
  widget$getValueJSMethod <- "getText"
  widget$setValueJSMethod <- "setText"
  widget$transportSignal <- NULL        # no transport
  widget$ExtConstructor <- "Ext.Button"
  widget$ExtCfgOptions <- function(.) {
    out <- list("text" = svalue(.))
    if(.$doAction())
      out[['text']] <- svalue(.$..action)
    ## add an icon
    text <- svalue(.)
    si <- getStockIcons()
    if(!is.na(si[text])) {
      out[['cls']] <- "x-btn-text-icon"
      out[['icon']] <- si[text]
    }

    return(out)
  }

  ## intercept action possibility
  ## XXX Issue here -- doesn't work with subwindows/
  widget$writeConstructor <- function(.) {
    ID <- .$asCharacter()
    if(.$doAction()) {
      out <- paste(sprintf("%s = new %s(%s);", ID, .$ExtConstructor, .$..action$asCharacter()),
                   sprintf("%s.id = %s;", ID, shQuote(.$ID)),
                   sprintf("%s.render(document.body);", ID),
                   sep="\n")
    } else {
      out <- get("writeConstructor",envir=EXTWidget)(.)
    }
    return(out)
  }

  
  ## add after CSS, scripts defined
  container$add(widget,...)

  if(!is.null(handler))
    widget$addHandlerClicked(handler=handler,action=action)
  
  invisible(widget)
}
  
