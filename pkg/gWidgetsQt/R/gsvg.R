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

## implement gsvg() in Qt, why not
setClass("gSvgQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )


## qtConstructor
creategwClass("QSvgWidget")

setMethod(".gsvg",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   filename = "",
                   width=480,
                   height=480,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)


#            w <- Qt$QSvgWidget()
            w <- gwQSvgWidget()
            
            obj = new("gSvgQt", block=w, widget=w,
              toolkit=toolkit, e=new.env(), ID=getNewID())
            w$setObject(obj)            # for click events
            
            size(obj) <- as.integer(c(width, height))
            svalue(obj) <- filename
            
            if(!is.null(container))
              add(container, obj,...)
            
            invisible(obj)
          })
          
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gSvgQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            tag(obj, "filename")
          })

## set file
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gSvgQt"),
                 function(obj, toolkit, index=NULL,  ..., value) {

                   w <- getWidget(obj)
                   ## set
                   w$load(value)
                   
                   ## store filename
                   tag(obj,"filename") <- value
                   return(obj)
                 })



## use EventWidget class for addHandlerXXX's
