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


setClass("gGroupQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            spacing <- getWithDefault(spacing, 0)
            theArgs <- list(...)                   # raise.on.dragmotion
            raise.on.dragmotion <- getWithDefault(theArgs$raise.on.dragmotion, FALSE)
            
            if(as.logical(horizontal))
              gp <- Qt$QHBoxLayout()
            else
              gp <- Qt$QVBoxLayout()

            ## put layout into a wudget
            gw <- Qt$QWidget()
            gw$setLayout(gp)

            obj <- new("gGroupQt",block=gw, widget=gp, toolkit=toolkit,            
                       e=new.env(), ID=getNewID()  
                       )
            

            
            ## implement scrollbars if asked. 
            if(use.scrollwindow == TRUE) {
              ## width <- getWithDefault(theArgs$width, 400L)
              ## height <- getWithDefault(theArgs$height, 400L)
              ## gw$setMinimumSize(width, height)

              ## JV: This doesn't work. It seems that I need to populate gw prior to placing
              ## it insde the scroll area so that the size is correct.
              
              sb <- Qt$QScrollArea()
              sb$setWidget(gw)


              obj@block <- sb
            }

            tag(obj, "horizontal") <- as.logical(horizontal)

            svalue(obj) <- spacing

            ## raise if we drag across
            if(as.logical(raise.on.dragmotion)) {
              
            }

            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })


##################################################
## methods
## padding
## Have two choices: setContentsMargin -- like padding in tcltk
## and setSpacing -- like padx in tcltk
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gGroupQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            lyt <- getWidget(obj)
            lyt$setSpacing(as.integer(value))
            
            return(obj)
          })

