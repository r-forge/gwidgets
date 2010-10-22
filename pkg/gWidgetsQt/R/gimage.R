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


## I should make an abstract class for gButton, gImage and gLabel
## instead I get lots of repeated code.


setClass("gImageQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## image use 
## inherits from glabel (for event handling)

setMethod(".gimage",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   filename = "", dirname="",
                   size="",
                   handler=NULL, action=NULL, 
                   container=NULL, ...) {

            force(toolkit)


#            lab <- Qt$QLabel()
            lab <- gwQLabel()

            
            obj = new("gImageQt", block=lab, widget=lab,
              toolkit=toolkit, e=new.env(), ID=getNewID())
            lab$setObject(obj)
            
            if(dirname != "" && dirname != "stock")
              file <- paste(dirname, filename, sep=.Platform$file.sep)
            else 
              file <- filename

            svalue(obj) <- file
            

            if(!is.null(handler)) {
              tag(obj, "handler.id") <-  addhandlerclicked(obj, handler=handler, action=action)
            }

            ## attach
            if(!is.null(container))
              add(container, obj,...)
            
            invisible(obj)
          })
          
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gImageQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            return(tag(obj,"filename"))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gImageQt"),
                 function(obj, toolkit, index=NULL,  ..., value) {

                   w <- getWidget(obj)
                   pix <- Qt$QPixmap()

                   if(file.exists(value)) {
                     pix$load(value)
                   } else if(!is.null(icon <- getStockIconFromName(value))) {
                     theArgs <- list(...)
                     icon.size <- as.integer(getWithDefault(theArgs$icon.size, c(32,32)))
                     pix <- Qt$QPixmap(icon$pixmap(icon.size[1], icon.size[2]))
                   } 
                   
                   w$setPixmap(pix)
                     
                   ## store filename
                   tag(obj,"filename") <- value
                   return(obj)
                 })


## handlers inherited from gEventWidget
