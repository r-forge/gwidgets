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

##' ##' Graphics device
##' Should use qtdevice(?) But can't get that to compile

setClass("gGraphicsQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## qt constructor

setMethod(".ggraphics",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,
                   dpi=75, ps=12,
                   container=NULL,...) {

            force(toolkit)

            msg <- "No embeddable\ngraphics device\navailable"
            ## take@widget to get glabel instance after going through gWidgets
            out <- .glabel(toolkit, msg, container=container)
            return(out)
          })


### methods

## raise this device
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
                 function(obj, toolkit, ..., value) {
                   if(is.logical(value) == TRUE) {
                     dev.set(tag(obj,"device"))
                   }
                   return(obj)
                 })

## save Current Page
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   gwCat("svalue not implemented\n")
                   return(obj)
                 })


### handlers ## these are from event framework, not signal
## add this expose event for graph
setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"expose-event",handler,action)
          })


## applies a handler to the mouse click. The handler gets extra
## argument h$x, h$y passed into it. These are in [0,1] coordinates
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGraphicsQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
          })

