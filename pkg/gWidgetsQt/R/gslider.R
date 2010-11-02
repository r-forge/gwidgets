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

setClass("gSliderQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

setMethod(".gslider",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   from=0, to=100, by = 1,
                   value=from,
                   horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)

            ## slider is for integer only
            if(length(from) == 1) {
              x <- seq(from, to, by)
            } else {
              x <- from
            }

            
            x <- sort(unique(x))
  
            if(as.logical(horizontal)) {
              ## ENUMS
              slider <- Qt$QSlider()
              slider$setOrientation(Qt$Qt$Horizontal)
              slider$setTickPosition(Qt$QSlider$TicksBelow) 
            } else {
              slider <- Qt$QSlider(Qt$Qt$Vertical)
              slider$setTickPosition(Qt$QSlider$TicksLeft) 
            }
            
            slider$setMinimum(1L)
            slider$setMaximum(length(x))
            slider$setPageStep(1+floor(length(x)/5))
            slider$setSingleStep(1L)
            
            obj <- new("gSliderQt",block=slider, widget=slider,
                       toolkit=toolkit)
            
            tag(obj, "..byIndexValues") <- x
            svalue(obj) <- value[1]
            
            ## don't know how to get tooltip, or some such, without reimplemetingn paint
            addhandlerchanged(obj, handler=function(h,...) {
              tooltip(obj) <- format(svalue(obj), digits=3)
            })
            tooltip(obj) <- format(svalue(obj), digits=3)
            
            
            if(!is.null(container))
              add(container, obj,...)
            
            if (!is.null(handler))  {
              id <- addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gSliderQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            ind <- w$value
            if(!is.null(index) && index)
              return(ind)
            else
              return(tag(obj, "..byIndexValues")[ind])
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gSliderQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(is.null(index) || !index) {
                     ## value is a value, must match
                     value <- as.character(match(value, tag(obj, "..byIndexValues")))
                   }
                   value <- value[1]
                   
                   w <- getWidget(obj)
                   if(!is.na(value)) {
                     value <- as.integer(value)
                     n <- length(tag(obj, "..byIndexValues"))
                     if(value >= 1L && value <= n)
                       w$setValue(value)
                   }

                   return(obj)
                 })



setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gSliderQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tag(x, "..byIndexValues")
          })

## Method to replace values of spin button
setReplaceMethod("[",
                 signature(x="gSliderQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##' replace values.
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gSliderQt"),
          function(x, toolkit, i, j, ..., value) {
            if(!missing(i) || !missing(j)) {
              cat(gettext("No replacement for single values"))
              return()
            }
            
            obj <- x
            widget <- getWidget(obj)
            curValue <- svalue(obj)

            value <- sort(unique(value))
            tag(obj, "..byIndexValues") <- value
            
            widget$setMinimum(1L)
            widget$setMaximum(length(value))

            svalue(obj) <- curValue


            ## all done
            return(obj)
          })




### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gSliderQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(newValue, h, ...) {
              h$value <- newValue
              handler(h)
            }
            ID <- .addhandler(obj, toolkit, "valueChanged", f, action, ...)
            return(ID)
          })


