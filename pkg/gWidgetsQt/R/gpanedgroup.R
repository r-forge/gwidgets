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

setClass("gPanedgroupQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

## TODO: method obj[1 or 2 ] <- replacewidget

setMethod(".gpanedgroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   widget1, widget2, horizontal=TRUE, container=NULL, ...) {
            ## add a paned group
            
            force(toolkit)
            
            pg <- Qt$QSplitter()
            ## Use Qt$ORientation$Horizontal (1) or $Vertical (2)
            if(as.logical(horizontal))
              pg$setOrientation(Qt$Qt$Horizontal)
            else
              pg$setOrientation(Qt$Qt$Vertical)
            
            ## make object -- note block is pg so that add works correctly
            ## as it calls getBlock(container)
            obj = new("gPanedgroupQt", block=pg, widget=pg,
              toolkit=toolkit,ID=getNewID(), e = new.env())
            

            if(!is.null(container))
              add(container, obj, ...)
            
            if(!missing(widget1) && !is.null(widget1))
              add(obj, widget1)
            if(!missing(widget2) && !is.null(widget2))
              add(obj, widget2)
            
            return(obj)
          })


## add -- use this rather than at construction time
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gPanedgroupQt", value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            w <- getWidget(obj)
            child <- getBlock(value)

            
            if(noChildren(obj) == 2) {
              gwCat("Already have 2 children")
              return()
            }

            
            theArgs <- list(...)
            expand <- getWithDefault(theArgs$expand, FALSE)
            anchor <- getWithDefault(theArgs$anchor, c(0,0))

            if(as.logical(expand))
              child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                  Qt$QSizePolicy$Expanding)

            if(is(child,"QWidget"))
              w$addWidget(child)
            
            
            setParent(value, obj)
            addChild(obj, value)
            
          })

## ## delete means we can readd -- in this case we actually dispose, as
## ## the widget doesn't get added back?
## setMethod(".delete",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gPanedgroupQt",
##                     widget="gWidgetQt"),
##           function(obj, toolkit, widget, ...) {
            

##           })



## svalue refers to sash position between 0 and 1
## sashpos
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gPanedgroupQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            sz <- w$sizes()
            sz[1]/sum(sz)
          })

## svalue sets position
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gPanedgroupQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(0 <= value && value <= 1) {
                     w <- getWidget(obj)
                     total <- sum(w$sizes())
                     out <- try(w$moveSplitter(as.integer(value * total), 0L), silent=TRUE)
                     if(inherits(out, "try-error"))
                       XXX("no moveSplitter method?")
                     
                   }
                   return(obj)
                 })
