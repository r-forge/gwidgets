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
            if(!all.equal(by, as.integer(by)) || !all.equal(from, as.integer(from))) {
              cat("Slider is for integer values only. Using a spinbutton")
              obj <- .gspinbutton(toolkit,
                           from, to, by, value, handler, action, container, ...)
              return(obj)
            }

            QtQSliderTickPosition <- c("noTicks"=0, ticksBothSides=3, ticksAbove=1, ticksBelow=2,
                                       ticksLeft=1, ticksRight=2)

            if(as.logical(horizontal)) {
              ## ENUMS
              ## XXX("How to get Qt$Orientations$Horizontal? Using numbers below")
              slider <- Qt$QSlider()
              slider$setOrientation(QtOrientation['horizontal'])
              slider$setTickPosition(QtQSliderTickPosition['ticksBelow']) # below
            } else {
              slider <- Qt$QSlider(QtOrientation['vertical'])
              slider$setTickPosition(QtQSliderTickPosition['ticksLeft']) # left
            }
            slider$setTickInterval(as.integer(5*by))
            
            obj <- new("gSliderQt",block=slider, widget=slider,
                       toolkit=toolkit)

            slider$setMinimum(as.integer(from))
            slider$setMaximum(as.integer(to))
            slider$setPageStep(as.integer(by))
            slider$setSingleStep(as.integer(by))
            svalue(obj) <- value
            
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
            w$value
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gSliderQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$setValue(as.integer(value[1]))
                   
                   return(obj)
                 })



## Method to replace values of spin button
setReplaceMethod("[",
                 signature(x="gSliderQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gSliderQt"),
          function(x, toolkit, i, j, ..., value) {
            obj <- x
            w <- getWidget(obj)

            ## check that value is a regular sequence
            if(length(value) <=1) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(obj)
            }
            if(length(value) > 2 &&
               !all.equal(diff(diff(value)), rep(0, length(value) - 2))) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(obj)
            }
            ## get current value, increment
            curValue <- svalue(obj)
            inc <- head(diff(value), n=1)
            tol <- sqrt(.Machine$double.eps) * 10
            if(!all.equal(inc, as.integer(inc + tol))) {
              warning("Increment must be an integer")
            }

            w$setMinimum(as.integer(min(value)))
            w$setMaximum(as.integer(max(value)))
            w$setSingleStep(as.integer(inc))
            w$setPageStep(as.integer(inc))
            svalue(w) <- curValue
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
