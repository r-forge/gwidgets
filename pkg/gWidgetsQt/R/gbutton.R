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

## setClass("gButtonQt",
##          contains="gComponentQt",
##          prototype=prototype(new("gComponentQt"))
##          )

setClass("gButtonQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## qtConstructor
creategwClass("QPushButton")

##' Can do keyboard shortcuts: just put & in button name, as in "&File".
##' The Alt-F will call clicked handler.
setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            force(toolkit)

            theArgs <- list(...)

            ## look like label if border=FALSE
            if(border == FALSE) {
              return(.glabel(toolkit, text,handler=handler,action=action,container=container,...))
            }


##            button <- Qt$QPushButton()
            button <- gwQPushButton()
            
            ## properties
            obj <- new("gButtonQt",
              block=button, widget=button, toolkit=toolkit,  e=new.env(), ID=getNewID())
            button$setObject(obj)

            tag(obj, "default_fill") <- "y" # don't like expansion in x direction
            
            svalue(obj) <- text
            
            ## add gp to container
            if(!is.null(container))
              add(container, obj, ...)

            
            ## add handler
            if (!is.null(handler)) 
              tag(obj,"handler.id") <- addhandlerchanged(obj, handler, action)

            
            invisible(obj)
          })


## constructor for action=gaction_instance
setMethod(".gbutton",signature(action="guiWidget", toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            .gbutton(toolkit, text, border, handler, action@widget, container, ...)
          })

## constructor for action=gaction_instance
setMethod(".gbutton",signature(action="gActionQt", toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            obj <- .gbutton(toolkit, container=container, ...)

            b <- getWidget(obj)
            a <- getWidget(action)

            b$addAction(a)

            ## need to set up connection between b and action
            ## leave icon coming from text, could do otherwise
            svalue(obj) <- a$text
            qconnect(b,"clicked", function() a$trigger())
            qconnect(a, "changed", function(obj) {
              svalue(obj) <- a$text
              enabled(obj) <- a$enabled
            }, user.data=obj)

            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gButtonQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            w$text
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gButtonQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   value <- as.character(value)
                   w$setText(value)
                   icon <- getStockIconFromName(tolower(value))
                   if(!is.null(icon))
                     w$setIcon(icon)
                   else
                     w$setIcon(Qt$QIcon())
                   
                   return(obj)
                 })


setMethod(".defaultWidget",
          signature(toolkit="guiWidgetsToolkitQt",obj="gButtonQt"),
          function(obj, toolkit, ...) {
            w <- getWidget(obj)
            w$setDefault(TRUE)
            focus(obj) <- TRUE
          })

## button activated by mouse click, space bar (not return), or keyboard shortcut
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gButtonQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

