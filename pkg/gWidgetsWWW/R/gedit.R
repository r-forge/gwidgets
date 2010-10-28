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


## gedit
## svalue works
## svalue<- works
## autocomplete code not in Ext??? Use gcombobox for that.
## add handlerKeyPress works but value is passed in through h$context$key, not h$key as with gWidgets
## change handler called after change and losing focus.
gedit <- function (text = "", width = 25, coerce.with = NULL,
                   handler = NULL,  action = NULL, container = NULL, ...) {
  
  widget <- EXTComponentText$new(toplevel=container$toplevel,
                             ..width = width * 8, # 8 pixels per character?
                           ..coerce.with=coerce.with)
  class(widget) <- c("gEdit",class(widget))
  widget$setValue(value=text)
  

  ## CSS

  ## Scripts

  ## methods
  widget$getValueJSMethod = "getValue"
  widget$setValueJSMethod = "setValue"
  widget$transportSignal <- "change"
#  widget$transportSignal <- "keyup" ## this gets sent too often, but will addHandlerKeystroke work w/o?
  widget$ExtConstructor <- "Ext.form.TextField"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "enableKeyEvents"= TRUE)
    if(exists("..width", ., inherits=FALSE))
      out[['width']] <- .$..width
    return(out)
  }



  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

