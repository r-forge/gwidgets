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

## Right way is to extend EXT.Component
gimage <- function(filename = "", dirname = "",  size = "",
                   handler = NULL, action = NULL, container = NULL,...,
                   resizable =FALSE     # WWW option. Keep?
                   ) {

  if(!resizable) {
    widget <- EXTComponentNoItems$new(toplevel=container$toplevel)
    class(widget) <- c("gImage", class(widget))
  } else {
    widget <- EXTComponentResizable$new(toplevel=container$toplevel)
    class(widget) <- c("gImage","gWidgetResizable", class(widget))
  }

  ## append dirname if non empty
  if(dirname != "")
    filename <- String(dirname) + filename
  widget$setValue(value=filename)

  ## moved in js file
  ## widget$scripts <- function(.) {
  ##   f <- system.file("javascript","ext.ux.imageBox.js", package="gWidgetsWWW")
  ##   out <- paste(readLines(f, warn=FALSE), collapse="\n")
    
  ##   return(out)
  ## }
      
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
