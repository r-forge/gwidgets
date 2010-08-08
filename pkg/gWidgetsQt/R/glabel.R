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
              handler <- function(h,...) {
                ret <- ginput(svalue(h$obj), parent=h$obj)
                if(!is.na(ret))
                  svalue(h$obj) <- ret
              }
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
                val = ginput(message=gettext("Change label value to:"), text=svalue(h$obj),
                  title=gettext("Change text for label"), icon="question", parent=obj)
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

##' font method to add color then call next method
##' (Could use style she
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLabelQt"),
                 function(obj, toolkit, ..., value) {
                   ## take care of color then pass on
                   if("color" %in% names(value)) {
                     if(is.list(value))
                       col <- value$color
                     else
                       col <- value["color"]
                     cur <- svalue(obj)
                     cur <- gsub("<[^>]*>","",cur)    # strip off
                     svalue(obj) <- sprintf("<font color=%s>%s</font>", col, cur) 
                   }
                   ## call next
                   callNextMethod()
                 })
