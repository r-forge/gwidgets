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

## WHy do we have to do this ourselves??? Must be a Qt thing
## expander group, like a group, only expands, contracts if requested
## inherits from ggroup, see ggroup's arguments: horizontal, spacing, container

##' Class for an expanding container
##' Have issue with resizing of parent widget when collapsed
##' 
qsetClass("ExpandContainer", Qt$QWidget, function(title="", parent=NULL) {
  super(parent)

  
  setSizePolicy(Qt$QSizePolicy$Preferred, Qt$QSizePolicy$Minimum) 
  
  this$cb <- Qt$QCheckBox(title)
  cb$setCheckState(Qt$Qt$Unchecked)
  
  this$centralWidget <- Qt$QGroupBox()

  
  lyt <- Qt$QVBoxLayout()
  super("setLayout", lyt)

  lyt$addWidget(cb, stretch=0, Qt$Qt$AlignTop | Qt$Qt$AlignLeft)
  lyt$addWidget(centralWidget, stretch=10, Qt$Qt$AlignTop | Qt$Qt$AlignLeft)

  qconnect(cb, "stateChanged", function(state) {
    setHidden(!as.logical(state & Qt$Qt$Checked))
  })

  setHidden(FALSE)
})

## checkbox methods
##' Return checkbox, used internally
qsetMethod("checkBox", ExpandContainer, function() cb)
##' label for expand group
qsetMethod("labelText", ExpandContainer, function() cb$text)
##' set label for expand group
##' @param txt label text
qsetMethod("setLabelText", ExpandContainer, function(txt) cb$setText(txt))

## push to central widget
##' override setLayout to put into central widget
qsetMethod("setLayout", ExpandContainer, function(lyt) centralWidget$setLayout(lyt))
##' returns state of widget (checked=open, unchecked=collapsed)
##' @returns Qt$Qt Check state object
qsetMethod("checkState", ExpandContainer, function() cb$checkState())
##' set checkstate with Qt$Qt Check State
qsetMethod("setCheckState", ExpandContainer, function(state) {
  setHidden(as.logical(Qt$Qt$Unchecked & state))
})

##' set hidden override. BOth hides and updates check button
##' @param bool TRUE to hide, FALSE to open
qsetMethod("setHidden", ExpandContainer, function(bool) {

  cb$blockSignals(TRUE)
  cb$setCheckState(ifelse(!bool, Qt$Qt$Checked, Qt$Qt$Unchecked)) # TRUE -> uncheck
  cb$blockSignals(FALSE)

  centralWidget$setHidden(bool)
  adjustSize()

  ## if(!is.null(parent <- this$parent()))
  ##   parent$adjustSize()
})



##' Class for gWidgets expand group
##' Inherits from ggroup the svalue method
setClass("gExpandgroupQt",
         contains="gGroupQt",
         prototype=prototype(new("gGroupQt"))
         )

##' toolkit constructor for expandgroup object
setMethod(".gexpandgroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", markup=FALSE,horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container = NULL, ...){

            force(toolkit)


            widget <- ExpandContainer()

            if(as.logical(horizontal))
              lyt <- Qt$QHBoxLayout()
            else
              lyt <- Qt$QVBoxLayout()
            widget$setLayout(lyt)
            
            obj <- new("gExpandgroupQt", block=widget, widget=lyt, 
                       toolkit=toolkit, e=new.env(), ID=getNewID())


            tag(obj, "default_fill") <- ifelse(horizontal, "y", "x") # orthogonal to expansion
            
            names(obj) <- text
            widget$setHidden(TRUE)    # don't expand initially

            
            if(!is.null(handler)) {
              addHandlerChanged(obj, handler=handler, action=action)
            }

            if(!is.null(container)) {
              add(container, obj, ...)
            }

            invisible(obj)
          })



##' Font is not implemented 
setReplaceMethod(".font",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit,  ..., value) {
            ### XXX implement me -- pass to cb
            return(obj)
          })

## svalue is inherited from ggroup. No longer does svalue<- set the name (which is deprecated)



## visible method
##' returns visible state
setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, set=TRUE,...) {
            getBlock(obj)$isVisible()
          })

##' control expand/close with logical
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
                 function(obj, toolkit, ..., value) {
                   getBlock(obj)$setHidden(!as.logical(value))
                   return(obj)
                 })


##' names refers to label
setMethod(".names",
          signature(toolkit="guiWidgetsToolkitQt",x="gExpandgroupQt"),
          function(x, toolkit) {
            getBlock(x)$labelText()
          })

##' replacement method for label
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x="gExpandgroupQt"),
                 function(x, toolkit, value) {
                   getBlock(x)$setLabelText(paste(value, collapse="\n"))
                   return(x)
                 })


## handlers
##' addhandler, goes onto checkbox
setMethod(".addhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {
            widget <- getBlock(obj)
            cb <- widget$checkBox()
            .addhandler(cb, toolkit, signal, handler, action,...)
          })

##' only a click handler
setMethod(".addhandlerchanged", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })

##' toolkit click handler
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(state, h, ...) handler(h, state, ...)
            .addhandler(obj, toolkit, "stateChanged", f, action, ...)
          })

##' remove handler from checkbox
setMethod(".removehandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            widget <- getBlock(obj)
            cb <- widget$checkBox()
            cb$disconnect(ID)
            invisible()
          })

##' block handler. Blocks *all* of them
setMethod(".blockhandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            widget <- getBlock(obj)
            cb <- widget$checkBox()
            cb$blockSignals(TRUE)
          })

##' unblock  blocked handlers
setMethod(".unblockhandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            widget <- getBlock(obj)
            cb <- widget$checkBox()
            cb$blockSignals(FALSE)
          })

