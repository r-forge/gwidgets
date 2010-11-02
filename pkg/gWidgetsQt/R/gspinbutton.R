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

## Could make spinbutton slider, subclass as methods are identical
setClass("gSpinbuttonQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

setMethod(".gspinbutton",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   from=0,to=10,by=1,value=from,digits=0,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)

            ## coerce to integer if meant to be
            if(isTRUE(all.equal(from, as.integer(from))))
              from <- as.integer(from)
            if(isTRUE(all.equal(to, as.integer(to))))
              to <- as.integer(to)
            if(isTRUE(all.equal(by, as.integer(by))))
              by <- as.integer(by)
            
            if(is.integer(by) && is.integer(from))
              sb <- Qt$QSpinBox()
            else
              sb <- Qt$QDoubleSpinBox()

            ## digits?
##            if(digits !=0)
##              XXX("implement digits")
            
            obj = new("gSpinbuttonQt",block=sb, widget=sb,
              toolkit=toolkit)

            sb$setMinimum(from)
            sb$setMaximum(to)
            sb$setSingleStep(by)

            svalue(obj) <- value

            sb$setWrapping(TRUE)        # wrap around
                                      
            if(!is.null(container))
              add(container, obj,...)
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gSpinbuttonQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            w$value
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gSpinbuttonQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$setValue(value)

                   return(obj)
                 })


## Method to replace values of spin button
setReplaceMethod("[",
                 signature(x="gSpinbuttonQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gSpinbuttonQt"),
          function(x, toolkit, i, j, ..., value) {
            w <- getWidget(x)

            ## check that value is a regular sequence
            if(length(value) <=1) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(x)
            }
            if(length(value) > 2 &&
               !isTRUE(all.equal(diff(diff(value)), rep(0, length(value) - 2)))) {
              warning("Can only assign a vector with equal steps, as produced by seq")
              return(x)
            }
            ## get current value, increment
            curValue <- svalue(x)
            inc <- head(diff(value), n=1)

            w$setMinimum(min(value))
            w$setMaximum(max(value))
            w$setSingleStep(inc)
            w$setValue(curValue)
                         

            ## all done
            return(x)
          })



### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gSpinbuttonQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(newValue, h, ...) {
              h$value <- newValue
              handler(h)
            }
            ID <- .addhandler(obj, toolkit, "valueChanged", f, action, ...)
            return(ID)
          })
