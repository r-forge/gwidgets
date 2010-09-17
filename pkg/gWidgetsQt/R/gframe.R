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

setClass("gFrameQt",
         contains="gGroupQt",
         prototype=prototype(new("gGroupQt"))
         )

## add a frame for packing. subclass of gGroup
setMethod(".gframe",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text = "", markup=FALSE,
                   pos = 0, ## pos in [0,1] 0 for left, (.01,.99) center, 1 for right
                   horizontal=TRUE,
                   container=NULL,
                   ...) {

            force(toolkit)
            



            
            f <- Qt$QGroupBox()
            if(pos < 0.33)
              f$setAlignment(Qt$Qt$AlignLeft)
            else if(pos > .66)
              f$setAlignment(Qt$Qt$AlignRight)
            else
              f$setAlignment(Qt$Qt$AlignHCenter)

            if(as.logical(horizontal))
              lyt <- Qt$QHBoxLayout()
            else
              lyt <- Qt$QVBoxLayout()
            f$setLayout(lyt)
            
            obj <- new("gFrameQt",
              block=f, widget=lyt, toolkit=toolkit)

            ## label
            if(markup) {
              XXX("Markup is implicit")
            }
            names(obj) <- text

            ## padding
            theArgs <- list(...)
            
            svalue(obj) <- getWithDefault(theArgs$spacing, 0)

            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })

## methods
## should be same as from ggroup:
## svalue<- padding
## names<- name

## ## sets the padding. Same as ggroup
## setReplaceMethod(".svalue",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gFrameQt"),
##           function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
##             ## adds some breathing room to object
##             ## value is pixels
##             return(obj)
##           })

## should put in a names argument to change label value
## return label name
setMethod(".names",signature(toolkit="guiWidgetsToolkitQt",
                             x="gFrameQt"),
          function(x, toolkit) {
            w <- getBlock(x)
            w$title
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x = "gFrameQt"),
                 function(x,toolkit,value) {
                   w <- getBlock(x)
                   w$setTitle(as.character(value))

                   return(x)
                 })

setReplaceMethod(".font", signature(toolkit="guiWidgetsToolkitQt",
                                    obj="gFrameQt"),
          function(obj, toolkit, ..., value) {
            ## font applies to label
            ## XXX("font<- for gframe not supported. Use markup in names<-")
            return(obj)
          })

