gsvg <- function(f, width=480, height=400,
##                 handler = NULL, action = NULL,
                 container = NULL,...) {

  ## put this into code
  ## require(RSVGTipsDevice, quietly=TRUE, warn=FALSE)
  if(!bypassRequire("RSVGTipsDevice"))
    return(glabel(gettext("gsvg needs the RSVGTipsDevice package to be installed"), cont=container))

  widget <- EXTComponent$new(toplevel=container$toplevel,
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
        "var el = document.getElementById('svg" + .$ID + "');" + "\n" +
          "el.innerHTML =  '<embed src=\"" + value + "\" " +
            "width=" + .$..width + " " +
              "height=" + .$..height + " " +
                "type=\"image/svg+xml\">';"
      cat(out)
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


## ggraphics is a pass through for gcanvas
ggraphics <- function(width = 480, height=400, container=NULL, ...) {
  gcanvas(width=width, height=height, container=container, ...)
}
