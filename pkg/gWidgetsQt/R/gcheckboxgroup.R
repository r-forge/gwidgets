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

## build widget based on gcheckbox
## This could be improved: [<- isn't right (handlers,...)
## merge changes back into gWidgetstcltk ("ANY"?)
setClass("gCheckboxgroupQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

setMethod(".gcheckboxgroup",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items, checked = FALSE,
                   horizontal=FALSE, 
                   handler = NULL, action = NULL, container = NULL, ...) {

            force(toolkit)

            if(missing(items) || length(items) == 0)
              stop("Need items to be a vector of items")


            ## this dispatch to happen on value of use.table value
            theArgs <- list(...)
            use.table <- getWithDefault(theArgs$use.table, FALSE)
            if(use.table) {
              obj <- .gcheckboxgrouptable(toolkit,
                                   items, checked, 
                                   handler=handler, action=action,
                                   container=container, ...)
              
              return(obj)
            }
              
            checked = rep(checked, length(items))

            group = ggroup(horizontal = horizontal, container=container, ...)
            
            lst = list()
            n = length(items)
            for(i in 1:n) {
              newItem = gcheckbox(items[i], checked=checked[i], cont=group)
              lst[[ as.character(items[i]) ]] = newItem
            }
  

            theArgs = list(...)
            if(!is.null(theArgs$coerce.with)) {
              coerce.with = theArgs$coerce.with
            } else {
              if(is.numeric(items))
                coerce.with = as.numeric
              else
                coerce.with = as.character
            }
            if(is.character(coerce.with))
              coerce.with <- get(coerce.with)

            
            ## make combination widget with all the values
            obj = new("gCheckboxgroupQt", block=group, widget=group,
              toolkit=toolkit,  e=new.env(), ID=getNewID())
  
            tag(obj, "items") <- items
            tag(obj, "itemlist") <- lst
            tag(obj, "coerce,with") <- coerce.with
            
            if(!is.null(handler))
              tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)

            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...)
            
            lst = tag(obj, "itemlist")
            vals = sapply(lst, svalue)         # logicals

            if(!is.null(index) && index == TRUE) {
              return(which(vals))       # return indices
            } else {
              vals = tag(obj,"items")[vals]
              coerce.with = tag(obj, "coerce.with") #obj@coercewith
              if(is.null(coerce.with))
                return(vals)
              else
                return(coerce.with(vals))
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   lst <- tag(obj,"itemlist")
                   n <- length(obj)
                   
                   ## compute values -- logical vector with length n
                   if(!is.null(index) && index) {
                     ## indices
                     values <- rep(FALSE, n)
                     values[value] <- TRUE
                   } else if(!is.logical(value)) {
                     ## characters
                    ind <- match(value, obj[])
                    ind <- ind[!is.na(ind)]
                    values <- rep(FALSE,length=n)
                    values[ind] <- TRUE
                   } else {
                     ## logical vector, we recycle
                     values = rep(value, length.out=n) ## recycle
                   }

                   sapply(1:n, function(i) svalue(lst[[i]]) <- values[i])
                   
                   return(obj)
                 })

## [ and [<- refer to the names -- not the TF values

setMethod("[",
          signature(x="gCheckboxgroupQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items <- tag(x,"items")
            if(missing(i))
              return(items)
            else
              return(items[i])
          })

## assigns names
setReplaceMethod("[",
                 signature(x="gCheckboxgroupQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupQt"),
          function(x, toolkit, i, j, ..., value) {
            items <- tag(x,"items")
            lst <- tag(x,"itemlist")
            n <- length(items)

            if(missing(i))
              i <- 1:length(items)
  
            if(is.logical(i))
              i = which(i)
            items[i] = value
            
            sapply(1:n, function(i) 
                   lst[[i]][] <- items[i]
                   )
            tag(x,"items") <- items
            tag(x,"itemlist") <- lst
  
             return(x)
          })


setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupQt"),
          function(x,toolkit) {
            length(tag(x,"items"))
          })


## inherited enabled isn't workgin                
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
                 function(obj, toolkit, ..., value) {

                   sapply(tag(obj,"itemlist"), function(i)
                          enabled(i,...) <- value)
                   return(obj)
                 })


## Handlers must just pass down to each item in the list.

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            IDs <- lapply(tag(obj,"itemlist"),function(i) {
              ## pass in obj to actualobj
              addHandlerChanged(i,handler=handler,action=action, actualobj = obj)
            })
            return(IDs)
          })
## clicked is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action, ...)
          })


setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            tag(obj,"handlerList") <- NULL
            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   removehandler(lst[[i]], ID[[i]])
                 )
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {

            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   blockhandler(lst[[i]], ID[[i]])
                   )
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupQt"),
          function(obj, toolkit, ID=NULL, ...) {
            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
              unblockhandler(lst[[i]], ID[[i]])
            )
          })

##################################################
##################################################
### Checkbox group in a table
setClass("gCheckboxgroupTableQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

setGeneric(".gcheckboxgrouptable", function(toolkit, items, checked=FALSE,
                                            handler=NULL, action=NULL,
                                            container=NULL, ...)
           standardGeneric(".gcheckboxgrouptable"))

setMethod(".gcheckboxgrouptable",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items, checked = FALSE,
                   handler = NULL, action = NULL, container = NULL, ...) {

            force(toolkit)

            tbl <- Qt$QTableWidget()
            tbl$setColumnCount(1)
            ## alternate shading
            tbl$setAlternatingRowColors(TRUE)
            ## stretch last section
            header <- tbl$horizontalHeader()
            header$setStretchLastSection(TRUE)
            ## no visible headers
            tbl$verticalHeader()$setVisible(FALSE)
            tbl$horizontalHeader()$setVisible(FALSE)
            
            
            ## make combination widget with all the values
            obj = new("gCheckboxgroupTableQt", block=tbl, widget=tbl,
              toolkit=toolkit,  e=new.env(), ID=getNewID())

            obj[] <- items       
            svalue(obj) <- checked
            
            
            if(!is.null(handler))
              tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)

            if(!is.null(container))
              add(container, obj, ...)
            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupTableQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tbl <- getWidget(obj)
            
            n <- tbl$rowCount
            if(n == 0)
              return(logical(0))
            
            vals <- sapply(1:n, function(i) {
              item <- tbl$item(i-1, 0)
              as.logical(item$checkState()) ## 0 -> FALSE, 2 -> TRUE
            })

            index <- getWithDefault(index, FALSE)

            if(index) {
              return(which(vals))       # return indices
            } else {
              obj[vals]                 # values from labels, not T,F
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupTableQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   
                   n <- length(obj)
                   if(n == 0)
                     return(obj)

                   tbl <- getWidget(obj)
                   
                   index <- getWithDefault(index, is.numeric(value))
                   if(index) {
                     tmp <- rep(FALSE, n)
                     tmp[value] <- TRUE
                     value <- tmp
                   }
                   ## recycle
                   value <- as.logical(rep(value, length=n))
                   state <- sapply(value, function(i) ifelse(i, Qt$Qt$Checked, Qt$Qt$Unchecked))
                   sapply(1:n, function(i) {
                     item <- tbl$item(i-1, 0)
                     item$setCheckState(state[i])
                   })
                   
                   return(obj)
                 })

## [ and [<- refer to the names -- not the TF values
## Here we can have a vector of names -- or a data frame
## 1st column names, 2nd icon, third tooltip -- like gcombobox
setMethod("[",
          signature(x="gCheckboxgroupTableQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupTableQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tbl <- getWidget(x)

            if(length(x) == 0)
              return(character(0))

            items <- sapply(seq_len(length(x)), function(i) {
              item <- tbl$item(i-1, 0)
              item$text()
            })

            if(missing(i))
              return(items)
            else
              return(items[i])
          })

## assigns names
setReplaceMethod("[",
                 signature(x="gCheckboxgroupTableQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupTableQt"),
          function(x, toolkit, i, j, ..., value) {
            ## value can be a vector or data frame
            ## if a data.frame we have
            ## text, stockicon, tooltip
            if(is.matrix(value))
              value <- data.frame(value, stringsAsFactors=FALSE)
            else if(!is.data.frame(value))
              value <- data.frame(value, stringsAsFactors=FALSE)

            tbl <- getWidget(x)
            
            
            ## get i
            if(missing(i)) {
              tbl$setRowCount(nrow(value))
              i <- seq_len(nrow(value))
            }
            
            if(is.logical(i))
              i = which(i)

            ## set items
            tbl$clear()
            m <- nrow(value)
            if(m == 0)
              return(x)
            
            sapply(1:m, function(i) {
              item <- Qt$QTableWidgetItem(as.character(value[i,1]))
              flags <- Qt$Qt$ItemIsEditable | Qt$Qt$ItemIsUserCheckable | Qt$Qt$ItemIsEnabled
              item$setFlags(flags) 
              
              item$setCheckState(Qt$Qt$Unchecked) # default, adjust

              if(ncol(value) >= 2) {
                icon <- value[i,2]
                icon <- getStockIconFromName(icon)
                if(!is.null(icon)) 
                  item$setIcon(icon)
              }

              if(ncol(value) >= 3) {
                tooltip <- value[i,3]
                if(!is.null(tooltip))
                  item$setToolTip(tooltip)
              }

              tbl$setItem(i - 1, 0, item)
            })            
            
            return(x)
          })


setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gCheckboxgroupTableQt"),
          function(x,toolkit) {
            tbl <- getWidget(x)
            tbl$rowCount
          })



## Handlers must just pass down to each item in the list.
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler=handler,action=action,...)
          })
## clicked is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gCheckboxgroupTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(item, h) {
              handler(h)
            }
            h <- list(obj=obj, action=action)
            ID <- qconnect(getWidget(obj), "itemChanged", f, user.data=h)
            invisible(ID)
          })

