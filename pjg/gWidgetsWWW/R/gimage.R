## Right way is to extend EXT.Component
gimage <- function(filename = "", dirname = "",  size = "",
                   handler = NULL, action = NULL, container = NULL,...,
                   resizable =FALSE     # WWW option. Keep?
                   ) {

  if(!resizable) {
    widget <- EXTComponent$new(toplevel=container$toplevel)
    class(widget) <- c("gImage", class(widget))
  } else {
    widget <- EXTComponentResizable$new(toplevel=container$toplevel)
    class(widget) <- c("gImage","gWidgetResizable", class(widget))
  }

  ## append dirname if non empty
  if(dirname != "")
    filename <- String(dirname) + filename
  widget$setValue(value=filename)


  widget$scripts <- function(.) {
    f <- system.file("javascript","ext.ux.imageBox.js", package="gWidgetsWWW")
    out <- paste(readLines(f, warn=FALSE), collapse="\n")
    
    return(out)
  }
      
  widget$setValueJSMethod = "setValue"
  widget$getValueJSMethod = "setValue"
  widget$ExtConstructor <- "Ext.ux.imageBox"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[["value"]] = svalue(.)
    return(out)
  }
  if(size != "") size(widget) <- size
  
  ## add after CSS, scripts defined
  container$add(widget,...)

  ## no handler
  
  invisible(widget)
}
