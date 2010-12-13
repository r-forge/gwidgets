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


## others
## propert ..length is added

## TODO: from being a vecto argument and using indices for slider...

##' slider widget
##'
##' Shows sequence of values. Can modify the tooltip via the tooltipTemplate argument (hidden)
##' @param from starting point
##' @param to ending point
##' @param by step size
##' @param value initial value
##' @param horizontal orientation
##' @param handler called when slider moved
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @export
gslider <- function(from = 0, to = 100, by = 1, value = from,
                    horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ...) {

  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by,
                             ..horizontal=horizontal
                             )
  class(widget) <- c("gSlider",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"
  widget$..length = if(horizontal) 300 else 100
  ## modify this. It needs to have {0} somewhere
  widget$..tooltipTemplate <- getFromDots(..., var="tooltipTemplate", default="{0}")

  ## CSS
  
  
  ## methods
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"

  ## No methods in extjs to set the values (minValue, maxValue, increment) after construction
  ## so we can't implement [<- method

  ## coerce to numeric -- stores a value
  widget$assignValue <- function(., value) {
    .$..data <- as.numeric(value[[1]])
  }

  
  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = newValue;'
    return(out)
  }
  widget$ExtConstructor <- "Ext.Slider"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "increment" = .$..by,
                "minValue" =  .$..from,
                "maxValue" = .$..to,
                "enableKeyEvents"=TRUE,
                "vertical"= !.$..horizontal,
                ## This works with 3.3.0,
                plugins = String(sprintf("new Ext.slider.Tip({getText: function(thumb){return String.format('%s', thumb.value)}})", .$..tooltipTemplate))
                ## needed for 3.0.0
#                plugins = String("new Ext.ux.SliderTip()")
              )

    if(.$..horizontal)
      out[['width']] <- .$..length
    else
      out[['height']] <- .$..length

    return(out)
  }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}



