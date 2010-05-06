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


## editable has entry widget that can be edited
setClass("gDroplistQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

setMethod(".gdroplist",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items, selected = 1, # use 0 for blank
                   editable=FALSE,
                   coerce.with = NULL,
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...               # do.quote = TRUE for quote of answer
                   ) {

            force(toolkit)


            ## items can be vector of items or data frame with
            ## one col: items
            ## two cols: items, icons
            ## three cols: items, icons, tooltip
            ## four or more cols: toolkit specific

            

            ## cb
            cb <- Qt$QComboBox()
            if(as.logical(editable)) {
              cb$setEditable(TRUE)
              cb$setDuplicatesEnabled(FALSE)
            }
            
            ## process
            theArgs <- list(...)

            ## keep this, but don't advertise
            if(!is.null(theArgs$do.quote)) 
              coerce.with <- function(x) paste("'",x,"'",sep="") # no space
            ## min.size to set minimum number of characters
            min.size <- getWithDefault(theArgs$min.size, 10)
            cb$setMinimumContentsLength(min.size)

            
            obj <- new("gDroplistQt",block=cb, widget=cb,
              toolkit=toolkit, e = new.env(), ID=getNewID())

            tag(obj,"coerce.with") <- coerce.with

            obj[] <- items
            svalue(obj, index=TRUE) <- selected

            
            if(!is.null(handler)) {
              id <- addhandlerchanged(obj, handler, action)
              tag(obj, "handler.id") <- id
            }

            if(!is.null(container))
              add(container, obj, ...)


            invisible(obj)
          })
          

### methods
## value is for getting/setting the selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDroplistQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            
            ind <- w$currentIndex + 1   # 0-based, -1 if no selection
            index <- getWithDefault(index, FALSE)

            if(index)
              return(ind)
            
            if(ind == 0) {
              val <- NA
            } else if(w$editable || ind > 0) {
              item <- w$model()$item(ind - 1)
              val <- item$text()
            }
            val <- do.coerce(val, tag(obj, "coerce.with"))
            return(val)
          })

## set the displayed value to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gDroplistQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   ## nothing to do if no items
                   n <- length(obj)
                   if(n < 1)
                     return(obj)

                   w <- getWidget(obj)
                   index <- getWithDefault(index, FALSE)
                   editable <- w$editable

                   ## if index, set
                   if(!index && editable) {
                     w$setEditText(value)
                     return(obj)
                   }
                   
                   if(!index) {
                     ## find index of value
                     items <- obj[]
                     if(as.character(value) %in% as.character(items)) {
                       ind <- min(which(as.character(items) %in% as.character(value)))
                     } else {
                       ind <- 0       # no match
                     }
                   } else {
                     ind <- value
                   }
                   ## set by index,
                   if(ind > 0 && ind <= length(obj))
                     w$setCurrentIndex(ind-1)
                   else
                     w$setCurrentIndex(-1) # clear out selection
                   
                   return(obj)
                 })

setMethod("length",
          signature(x="gDroplistQt"),
          function(x) {
            .length(x, x@toolkit)
          })
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gDroplistQt"),
          function(x, toolkit) {
            w <- getWidget(x)
            val <- getWithDefault(w$count,0)              # 0 if no items
          })


## the methods [ and [<- refer to the pre-defined values in the drop list.
## [
setMethod("[",
          signature(x="gDroplistQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gDroplistQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            ## return vector of items -- not data frame
            
            n <- length(x)
            if(n == 0)
              return(character(0))

            w <- getWidget(x)
            mod <- w$model()
            items <- sapply(1:n, function(i) {
              item <- mod$item(i-1)
              item$text()
            })
            
            if(missing(i))
              return(items)
            else
              return(items[i])
          })


## replaces the values in droplist
## values is a vector of values -- not a dataframe
#set.values.gDropList = function(obj, values, ...) {
setReplaceMethod("[",
                 signature(x="gDroplistQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gDroplistQt"),
          function(x, toolkit, i, j, ..., value) {
            ## value can be a vector or data frame
            ## if a data.frame we have
            ## text, stockicon, tooltip
            if(is.matrix(value))
              value <- data.frame(value, stringsAsFactors=FALSE)
            else if(!is.data.frame(value))
              value <- data.frame(value, stringsAsFactors=FALSE)
            nc <- ncol(value)
            
            w <- getWidget(x)
            mod <- w$model()
            curIndex <- w$currentIndex + 1

            setItem <- function(mi, vi=mi) { # model index, value index
              val <- as.character(value[vi,1])
              item <- Qt$QStandardItem(val)
              if(nc >= 2) {
                icon <- getStockIconFromName(value[vi,2])
                if(!is.null(icon)) 
                  item$setIcon(icon)
              }
              if(nc >=3)
                item$setToolTip(value[vi,3])

              w$model()$setItem(mi-1, item)
                
              }

            if(missing(i)) {
              ## replace it all
              
              w$clear()
              if(nrow(value) == 0)
                return(x)

              lapply(seq_len(nrow(value)), setItem)
              
              ## set if possible
              if(curIndex > 0)
                w$setCurrentIndex(curIndex-1)
              w$update()
              
            } else {
              j <- min(length(i), nrow(value))
              for(k in 1:j) {
                setItem(i[k], k)
              }
            }

            return(x)
          })



###################################################
  
### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDroplistQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj,toolkit, handler,action)            
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDroplistQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            w <- getWidget(obj)

            ## "activated" (currentIndex reponds to user changed)
            f <- function(newValue, l, ...) {
              l$h$value <- newValue
              l$handler(l$h)
            }
            h <- list(obj=obj, action=action)
            ID <- qconnect(w,"activated",f,user.data=list(h=h, handler=handler))

            ## editable getes another signal too
            if(w$editable) {
              ID <- list(ID, qconnect(w, "editTextChanged", f, user.data=list(h=h,handler=handler)))
            }
            invisible(ID)
          })

## keystroke if editable
## XXX("Check this out")
setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDroplistQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            w <- getWidget(obj)
            if(w$editable)
              callNextMethod()
          })
