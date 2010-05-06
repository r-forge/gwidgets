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

## can't extend or shorten via []<- but o/w is working
## implement horizontal=TRUE

setClass("gRadioQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

## constructor
setMethod(".gradio",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items, selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,       
                   ...
                   ) {
            force(toolkit)

            if(is.data.frame(items))
              items <- items[,1, drop=TRUE]
            
            n = length(items)
            
            if (n<2)
              stop(gettext("Radio button group makes sense only with at least two items. Use a checkbox"))

            
            l <- list()                 # list of radio buttons


            w <- Qt$QWidget()

            if(as.logical(horizontal))
              lyt <- Qt$QHBoxLayout()
            else
              lyt <- Qt$QVBoxLayout()
            lyt$addStretch(1L)
            w$setLayout(lyt)
            


            
            obj = new("gRadioQt",block=w, widget=lyt,
              e=new.env(), ID=getNewID(),
              toolkit=toolkit)


            
            tag(obj, "items") <- l
            obj[] <- items
            svalue(obj, index=TRUE) <- as.integer(selected)

            ## use coerce with
            theArgs = list(...)
            if(!is.null(theArgs$coerce.with)) {
              coerce.with = theArgs$coerce.with
            } else {
              if(is.numeric(items))
                coerce.with = as.numeric
              else if(is.logical(items))
                coerce.with = as.logical
              else
                coerce.with = as.character
            }
            if(is.character(coerce.with))
              coerce.with = get(coerce.with)

            tag(obj, "coerce.with") <- coerce.with
            
            ## add to container
            if(!is.null(container))
              add(container,  obj,...)
  
            ## add handler
            if(!is.null(handler))
              tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)

            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            l <- tag(obj, "items")
            
            index <- as.integer(getWithDefault(index, FALSE))

            out <- sapply(l, function(i) i$isChecked())

            if(length(out) == 0)
              return()
            
            if(index) {
              return(which(out))
            } else {
              val <- do.coerce(obj[out], tag(obj,"coerce.with"))
              return(val)
            }
          })

## getWidget should return block
## getWidget is not a method, so we can't define it here.
## Rather, we make this a container

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   l <- tag(obj, "items")
                   
                   if(is.data.frame(value))
                     value <- value[,1, drop=TRUE]
                   index <- as.logical(getWithDefault(index, FALSE))
                   
                   items <- obj[]
                   
                   if(index) {
                     ind <- as.integer(value)
                     if(length(ind) == 0)
                       ind <- 1
                   } else {
                     value <- as.character(value)
                     items <- as.character(items)
                     
                     if(value %in% items) {
                       ind <- match(value, items)
                     } else {
                       XXX("No match")
                       ind <- -1
                     }
                   }
                   
                   if(ind > 0) {
                     if(ind < 1 || ind > length(items))
                       gwCat("Index out of range")
                     else
                       l[[ind]]$setChecked(TRUE)
                   }
                   
                   return(obj)
                 })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gRadioQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            ## return(items)
            items = tag(x,"items")
            items <- sapply(items, function(i) i$text)
            
            if(missing(i))
              items[,...,drop=drop]
            else
              items[i,...,drop=drop]
          })
            
setMethod("[",
          signature(x="gRadioQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })


## This sets the labels for the buttons
## add in markup here.
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gRadioQt"),
          function(x, toolkit, i, j, ..., value) {

            curVal = svalue(x, index=TRUE)
            n <- length(x)

            ## check
            if(is.data.frame(value))
              value <- value[,1,drop=TRUE] 
            
            if(missing(i))
              i <- length(value)
            
            if(length(value) != length(i)) {
              ## replace
              ## clear out layout, add from value -- use coerce.with still
              l <- tag(x, "items")
              lyt <- getWidget(x)
              sapply(l, function(i) {
                i$hide()
                lyt$removeWidget(i)
              })
              l <- list()
              ## add
              sapply(seq_along(value), function(i) {
                rb <- Qt$QRadioButton(as.character(value[i]))
                lyt$addWidget(rb)
                l[[i]] <<- rb
              })
              tag(x, "items") <- l

              ## we may have erased handler
              handler <- tag(x, "handler")
              if(!is.null(handler))
                tag(x, "handler.id") <-
                  .addhandlerclicked(x, toolkit, handler=handler, action=tag(x,"action"))
              
            }

            l <- tag(x, "items")

            sapply(seq_along(value), function(i) {
              l[[i]]$setText(value[i])
            })
            
            svalue(x, index=TRUE) <- curVal

            
            ## all done
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gRadioQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gRadioQt"),
          function(x,toolkit) {
            length(tag(x, "items"))
          })

## apply enabled to each
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
                 function(obj, toolkit, ..., value) {
                   l <- tag(obj, "items")
                   sapply(l, function(i) i$setEnabled(as.logical(value)))

                   return(obj)
                 })

## visible?

##################################################
## handlers



## click and changed the same
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj,toolkit,handler,action,...)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            l <- tag(obj, "items")
            tag(obj, "handler") <- handler
            tag(obj, "action") <- action
            ## put onto the items
            f <- function(checked, h, ...) {
              if(checked)
                handler(h,...)
            }
            h <- list(obj=obj, action=action)
            merge(h, list(...))
            IDs <- sapply(l, function(i) {
              qconnect(i, "toggled", f, user.data=h)
            })

            invisible(IDs)
          })


setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, ID=NULL, ...) {
            ## id is a lst of IDs to match that of 
            l <- tag(obj, "items")
            sapply(l, function(i) {
              if(is.null(ID)) {
                i$disconnect()
              } else {
                ## XXX(need to loop over ID to see if we mathc, ...)
                i$disconnect()
              }
            })
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, ID=NULL, ...) {
            ## id is a lst of IDs to match that of 
            l <- tag(obj, "items")
            sapply(l, function(i) {
              if(is.null(ID)) {
                i$blockSignals(TRUE)
              } else {
                for(j in ID) {
                }
                ## XXX need to loop over IDs
                i$blockSignals(TRUE)
              }
            })
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gRadioQt"),
          function(obj, toolkit, ID=NULL, ...) {
            ## id is a lst of IDs to match that of 
            l <- tag(obj, "items")
            sapply(l, function(i) {
              if(is.null(ID)) {
                i$blockSignals(FALSE)
              } else {
                for(j in ID) {
                  ###
                }
                ## XXX("look")
                i$blockSignals(FALSE)
              }
            })
          })
