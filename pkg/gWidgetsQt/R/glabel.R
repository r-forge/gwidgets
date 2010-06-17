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


## class in aabClass
## also in gImage, so put here
setClass("gLabelQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## qtConstructor
creategwClass("QLabel")

## constructor
setMethod(".glabel",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                   action = NULL, container = NULL, 
                   ...
                   ) {

            force(toolkit)

            if(markup) {
              ### XXX("Markup implemented natively") 
            }
            if(editable) {
              XXX("implement editable")
            }
            

#            l <- Qt$QLabel()
            l <- gwQLabel()
            
            obj <- new("gLabelQt",
                       block=l, widget=l,
                       toolkit=toolkit,
                       e=new.env(), ID=getNewID()
                       )
            l$setObject(obj)            # for events
            
            svalue(obj) <- text

            
            ## add to container
            if(!is.null(container))
              add(container, obj, ...)

            
            if(editable) {
              handler <- function(h,...) {
                val = ginput(message="Change label value:", text=svalue(h$obj),
                  title="Change text for label", icon="question", parent=obj)
                if(!is.na(val))
                  svalue(obj) <- val
              }
            }
            
            if(!is.null(handler)) {
              tag(obj, "handler.id") <- addhandlerclicked(obj, handler=handler,action=action)
            }
            
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gLabelQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            w$text
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gLabelQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$text <- as.character(value)
                   return(obj)
                 })
