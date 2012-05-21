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

##' widget to be used as a device. Uses \pkg{canvas} pacakge
##' @param f a file name
##' @param width width of widget in pixels
##' @param height heighto f widget in pixels
##' @param container parent container
##' @param ... passed to add method of container
##' @export
gcanvas <- function(f, width=480, height=400,
##                    handler = NULL, action = NULL,
                    container = NULL,...) {

  if(!bypassRequire("canvas"))
    return(glabel(gettext("gcanvas needs the canvas package to be installed"), container=container))

  
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..width=as.numeric(width),
                             ..height=as.numeric(height))
  
  class(widget) <- c("gCanvas",class(widget))
  if(!missing(f))
    widget$setValue(value=f)

  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    
    out[['html']] <- String() +
      '\'<canvas id="gWidgetsCanvas' + .$ID + '" width=' + .$..width + ' height=' + .$..height +
        '>' + gettext("If you see this, your browser does not support the canvas tag.") + '</canvas>\''

    out[["width"]] <- .$..width         # for panel size
    out[["height"]] <- .$..height
    
    return(out)
  }

  
  widget$footer <- function(.) {
    out <- String(sep="") +
      'var ctx = document.getElementById("gWidgetsCanvas' + .$ID + '").getContext("2d");' +
        'if(!ctx.fillText) {ctx.fillText =function() {};};' + '\n' +
          .$setValueJS() 
    return(out)
  }

  widget$setValueJS <- function(.,...) {
    if(exists("..data", envir=., inherits=FALSE)) {
      value <- .$..data
      out <- String()
      if(!is.null(value)) {
        ## clear out
        out <- out + sprintf("ctx.clearRect(0,0,%s,%s);", .$..width, .$..height) +
          paste(readLines(value, warn=FALSE)[-1], collapse="\n") +
            '\n'
      }
      return(out)
    } else {
      return("")
    }
  }

  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}


##' ggraphics is a pass through for gcanvas
##' @alias gcanvas
ggraphics <- function(width = 480, height=400, container=NULL, ...) {
  gcanvas(width=width, height=height, container=container, ...)
}
