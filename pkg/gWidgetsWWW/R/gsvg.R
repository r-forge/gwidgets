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


##' widget to display svg files
##'
##' Used like a non-interactive device: create file
##' (\code{getStaticTmpFil(ext=".svg")}), use this with the svg device
##' (such as \code{devSVGTips} from the \pkg{RSVGTipsDevice} package),
##' then pass to \code{f} or use \code{svalue<-} to assign.
##' @param f filename
##' @param width width of widget in pixels
##' @param height height of widget in pixels
##' @param container parent container
##' @param ... passed to \code{add} method of parent container
##' @export
gsvg <- function(f, width=480, height=400,
##                 handler = NULL, action = NULL,
                 container = NULL,...) {

  ## put this into code
  ## require(RSVGTipsDevice, quietly=TRUE, warn=FALSE)
  if(!bypassRequire("RSVGTipsDevice"))
    return(glabel(gettext("gsvg needs the RSVGTipsDevice package to be installed"), container=container))

  widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                             ..width=as.numeric(width),
                             ..height=as.numeric(height))
  
  class(widget) <- c("gSvg",class(widget))
  if(!missing(f))
    widget$setValue(value=f)

  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    
    out[['html']] <- paste(             # so we get quotes
                           "<div id=\"svg", .$ID, "\"></div>",
                           sep="")
    
    return(out)
  }

  
  widget$footer <- function(.) {
    out <- String(sep="\n") +
      .$setValueJS() 
    return(out)
  }

  widget$setValueJS <- function(.,...) {
    if(exists("..data", envir=., inherits=FALSE)) {
      ## need to write handlers here by munging svg file
      ## Issue here is the javascript code in the gWidgetsWWW page is not
      ## known to the SVG page so the following doesn't work
      ## One needs to write the AJAX call directly
      ## XXX Leaving this for later
      ##       if(exists("..handlers", envir=., inherits=FALSE)) {
      ##         allHandlers <- .$..handlers
      ##         handler <- allHandlers[[1]]

      ## XXX If we require XML add to dependencies for the package
      ##         require(XML, quietly=TRUE, warn=FALSE)
      ##         doc <- xmlParse(.$..data)
      ##         d <- xmlRoot(doc)
      ##         out <- String() +
      ##           'runHandlerJS(' + handler$handlerID + ",\'\',\'\');"
      ##         xmlAttrs(d[[4]]) <- c(onclick=as.character(out))
      ##         saveXML(doc, .$..data)
      ##       }
      
      value <- .$..data ## function name
      ## convert to URL -- it is in static directory
      value <- convertStaticFileToUrl(value)
      out <- String() +
        paste(sprintf("var el%s = document.getElementById('svg%s');", .$ID, .$ID),
              sprintf("el%s.innerHTML = '<embed src=\"%s\" width=%s height=%s type=\"image/svg+xml\">';",
                      .$ID,
                      escapeQuotes(value),
                      .$..width, .$..height),
              collapse="")
        ## "var el = document.getElementById('svg" + .$ID + "');" + "\n" +
        ##   "el.innerHTML =  '<embed src=\"" + value + "\" " +
        ##     "width=" + .$..width + " " +
        ##       "height=" + .$..height + " " +
        ##         "type=\"image/svg+xml\">';"
      .$addJSQueue(out)
      ## cat(out)
    } else {
      return("")
    }
  }

  ## Handler code needs to be written. This stub just ensures it isn't
  ## written out if specified.
  widget$writeHandlersJS <- function(., signal, handler=NULL) { return("")}

  ## XXX replace when handler code added
  ##   if(!is.null(handler)) 
  ##     widget$addHandlerClicked(handler, action)

  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}

