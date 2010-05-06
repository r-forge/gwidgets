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
setClass("gExpandgroupQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )


setMethod(".gexpandgroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", markup=FALSE,horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container = NULL, ...){

            force(toolkit)

            w <- Qt$QWidget()

            ## 3 widgets
            cb <- gcheckbox()
            XXX("Use QT Graphic Item, not label")
            label <- glabel(text)
            mw <- Qt$QWidget()

            elyt <- Qt$QVBoxLayout()
            w$setLayout(elyt)
            glty <- Qt$QGridLayout()
            elyt$addLayout(glty)
            elyt$addStretch(10)         # push glty up

            


            ### anchors into upper right"
            glty$addWidget(getBlock(cb), 0, 0,1,1)
            glty$addWidget(getBlock(label), 0, 1, 1, 1)
            glty$addWidget(Qt$QLabel(), 0, 2, 1, 1)
            glty$addWidget(mw, 1, 1,1,1)
            glty$addWidget(Qt$QLabel(), 2, 1,1,3)

            ## give space to cell 1,1
            glty$setColumnStretch(1, 1)
            glty$setRowStretch(0, 0)
            glty$setColumnStretch(0, 0)
            ## hacks -- really want to push alignment
            glty$setRowStretch(2, .1)            
            glty$setColumnStretch(2, .1)
            

            if(as.logical(horizontal))
              lyt <- Qt$QHBoxLayout()
            else
              lyt <- Qt$QVBoxLayout()
            mw$setLayout(lyt)
            
            obj <- new("gExpandgroupQt", block=w, widget=lyt, 
                       toolkit=toolkit, e=new.env(), ID=getNewID())


            tag(obj, "mainwidget") <- mw
            tag(obj, "cb") <- cb
            tag(obj, "label") <- label
            
            ## initial state is open
            visible(obj) <- TRUE
            
            ## must take care of closing/opening
            qconnect(getWidget(cb), "stateChanged", function(state, obj) {
              visible(obj) <- state
            }, user.data=obj)
            
            if(!is.null(handler)) {
              addHandlerChanged(obj, handler=handler, action=action)
            }


            

            if(!is.null(container)) {
              add(container, obj, ...)
            }

            invisible(obj)
          })



## push onto label
setReplaceMethod(".font",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit,  ..., value) {
            ## add value to expandgroup
            font(tag(obj,"label")) <- value
            return(obj)
          })


## Should make
## a) svalure refer to padding, ala ggroup padding
## b) names refer to label
## c) font refer to font of label
## d) visible refer to state

## value refers to padding
## FOr svalue<- we still accept non-numeric for setting lable
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            svalue(tag(obj,"label"))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt",
                           value = "numeric"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   ## If numeric, this is spacing -- from ggroup
                   svalue(obj@widget, value)
                   return(obj)
                 })

## set name, but is deprecated
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   gwCat("Using names<- to set label value")
                   names(obj) <- value
                   return(obj)
                 })


## visible method
setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, set=TRUE,...) {
            svalue(tag(obj,"cb"))
          })

## control expand/close with logical
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
                 function(obj, toolkit, ..., value) {
                   cb <- tag(obj, "cb")
                   mw <- tag(obj, "mainwidget")
                   
                   if(as.logical(value)) {
                     svalue(cb) <- TRUE
                     mw$show()
                   } else {
                     svalue(cb) <- FALSE
                     mw$hide()
                   }
                   
                   return(obj)
                 })


## names refers to label
setMethod(".names",
          signature(toolkit="guiWidgetsToolkitQt",x="gExpandgroupQt"),
          function(x, toolkit) {
            svalue(tag(x,"label"))
          })

setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x="gExpandgroupQt"),
                 function(x, toolkit, value) {
                   svalue(tag(x,"label")) <- as.character(value)
                   return(x)
                 })


## handlers
## putonto icon, button
setMethod(".addhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {
            l <- list(tag(obj, "cb"), tag(obj, "label"))
            IDs <- sapply(l, function(i) {
              .addhandler(i, toolkit,  handler, action,...)
            })
            IDs
          })

setMethod(".removehandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            l <- list(tag(obj, "cb"), tag(obj, "label"))
            sapply(l, function(i) {
              .removehandler(i, toolkit,  ID, ...)
            })
            invisible()
          })

setMethod(".blockhandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            l <- list(tag(obj, "cb"), tag(obj, "label"))
            sapply(l, function(i) {
              .blockhandler(i, toolkit,  ID, ...)
            })
            invisible()
          })

setMethod(".unblockhandler", 
          signature(toolkit="guiWidgetsToolkitQt",obj="gExpandgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            l <- list(tag(obj, "cb"), tag(obj, "label"))
            sapply(l, function(i) {
              .unblockhandler(i, toolkit,  ID, ...)
            })
            invisible()
          })
