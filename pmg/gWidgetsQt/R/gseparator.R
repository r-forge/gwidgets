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

setClass("gSeparatorQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## should this return object?
setMethod(".gseparator",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   horizontal = TRUE, container = NULL, ...) {

            force(toolkit)

            f <- Qt$QFrame()
            if(horizontal)
              f$setFrameShape(4L)       # Shape enumerations
            else
              f$setFrameShape(5L)
            f$setFrameShadow(32L)       # QFrame::sunken
            
            obj = new("gSeparatorQt", block=f, widget=f,
              toolkit=toolkit, ID=getNewID(), e = new.env())

            if(!is.null(container))
              add(container, obj, ...)


            invisible(obj)
            
          })

## no size method
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkitQt",obj="gSeparatorQt"),
                 function(obj, toolkit, ..., value) {
                   gwCat("No size<- method for separators")
                   return(obj)
                 })


## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkitQt", obj="gLayoutQt",
##                     value="gSeparatorQt"),
##           function(obj, toolkit, value, ...) {
##           })

.isgSeparator <- function(obj) {
  (is(obj,"guiComponent") && is(obj@widget,"gSeparatorQt") ) ||
    is(obj,"gSeparatorQt")
}



