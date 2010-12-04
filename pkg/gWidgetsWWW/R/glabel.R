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

glabel <- function(text = "", markup = FALSE, editable = FALSE,
                   handler = NULL, action = NULL, container = NULL,...) {

  widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                             ..editable = editable,
                             ..markup = markup
                             )
  
  class(widget) <- c("gLabel",class(widget))
  widget$setValue(value=escapeHTML(text))

  widget$setValueJSMethod = "setValue"
  widget$getValueJSMethod = "setValue"
  widget$ExtConstructor <- "Ext.ux.labelBox"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[["value"]] = unescapeURL(svalue(.))
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}
