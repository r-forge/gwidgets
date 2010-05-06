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

## StatusBar. Use value to push message, value to pop
## Qt adds "add" method for widgets
setClass("gStatusbarQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## constructor
setMethod(".gstatusbar",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", container=NULL, ...) {

            
            force(toolkit)            


            sb <- Qt$QStatusBar()

            
            obj = new("gStatusbarQt",block=sb, widget=sb,
              toolkit=toolkit, e=new.env(), ID=getNewID())

            tag(obj,"stack") <- c()

            svalue(obj) <- text
            
            ## add to container. Window only?
            if(!is.null(container))
              add(container, obj,...)

            
            invisible(obj)
          })

### methods

## This pops label
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gStatusbarQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            
            ## pop the stack
            stack <- tag(obj,"stack")
            val <- stack[1]
            if(length(stack))
              stack <- stack[-1]
            tag(obj,"stack") <- stack

            if(length(stack))
              value <- stack[1]
            else
              value <- ""

            w$clearMessage()
            w$showMessage(value)

            return(val)
          })


setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gStatusbarQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   
                   stack <- tag(obj,"stack")
                   stack <- c(as.character(value), stack)
                   tag(obj,"stack") <- stack
                   
                   w$clearMessage()
                   w$showMessage(value)

                   
                   return(obj)
                 })


## we can add a widget

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gStatusbarQt",value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            sb <- getWidget(obj)
            child <- getBlock(value)

            theArgs <- list(...)
            index <- getWithDefault(theArgs$index, 1)

            
            if(is(child,"QWidget"))
              sb$insertWidget(index, child)
          })

