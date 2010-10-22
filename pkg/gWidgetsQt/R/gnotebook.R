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

## TODO:

setMethod(".gnotebook",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   tab.pos = 3,                          # same as pos= in text
                   closebuttons = FALSE,
                   dontCloseThese = NULL,                # integer of tabs not to close
                   container=NULL,                       # add to this container
                   ...) {
            
            force(toolkit)

            
            nb <- Qt$QTabWidget()
            
            ## create gnotebook object
            obj = new("gNotebookQt", block=nb, widget=nb,
              toolkit=toolkit, e = new.env(), ID=getNewID(),
              closebuttons = as.logical(closebuttons),
              dontCloseThese = ifelse(is.null(dontCloseThese),0,dontCloseThese)
              )

            ## properties: Qt:TabPosition Enum
            RtoTabPositionEnum <- as.integer(c(1,2,0,3))
            nb$setTabPosition(RtoTabPositionEnum[tab.pos])
            
            if(closebuttons) {
              nb$setTabsClosable(TRUE)
              ## connect to signal
              qconnect(nb, "tabCloseRequested", function(index) {
                if(is.null(dontCloseThese) ||
                   !((index + 1)  %in% dontCloseThese))
                  nb$removeTab(index)
              })
            }
            nb$setUsesScrollButtons(TRUE)

            

            ## add to container
            if(!is.null(container))
              add(container, obj, ...)

            
            invisible(obj)
          })

### methods
## return the current tab number
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            nb <- getWidget(obj)
            nb$currentIndex + 1
          })

## set the current tab to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   nb <- getWidget(obj)
                   n <- length(obj)

                   value <- max(1,min(value,n))
                   nb$setCurrentIndex(value - 1)
                   
                   return(obj)
                 })


## remove the current tab
## this should be called delete -- which is used to remove objects
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
          function(obj, toolkit,  ...) {

            nb <- getWidget(obj)
            
            theArgs = list(...)
            to.right=ifelse(!is.null(theArgs$to.right), theArgs$to.right,FALSE)
            dontCloseThese = obj@dontCloseThese
            if(dontCloseThese == 0) dontCloseThese = NULL
            deleteOK = function(i) {
              if(is.null(dontCloseThese)) return(TRUE)
              if(i %in% dontCloseThese) return(FALSE)
              return(TRUE)
            }
            cur.pageno = svalue(obj)

            ## we clear out the current page unless there is more!
            inds <- 0
            if(to.right) {
              n <- length(obj)
              no.right <- n - cur.pageno
              if(no.right > 0) 
                inds <- no.right:0      # must work from last backwards
            }
            ## clear out would like "hide" here, as then we can
            ## readd. Not working here? why not?
            for(i in inds) {
              j <- cur.pageno + i
              if(deleteOK(j)) {
                gwidget <- obj[j]
                if(!inherits(gwidget, "try-error")) {
                  removeChild(obj, gwidget)
                  removeParent(gwidget)
                }
                nb$removeTab(j-1)
              }
            }

            if(cur.pageno > 0) {        # error if no pages
              if(cur.pageno <= length(obj))
                svalue(obj) <- cur.pageno
              else
                svalue(obj) <- length(obj)
            }
          })


setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
          function(obj, toolkit, widget, ...) {
            nb <- getWidget(obj)
            widget <- getBlock(widget)
            if(is(widget, "QWidget")) {
              i <- nb$indexOf(widget)
              nb$removeTab(i)
            }
          })



### add() is a workhorse method here. Several args available in ...
#add.gNotebook = functionf(obj, value,
#  label="", markup = FALSE,
#  index = NULL, override.closebutton = FALSE, ...) {
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt",
                    value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj, toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt",
                    value="gWidgetQt"),
          function(obj, toolkit, value,  ...) {


            ## in ... we have many possibilies
            ## label -- for setting label  (also look for name)
            ## index for setting the index of page to add
            ## markup -- markup label
            ## override.closebutton -- to not put closebutton even if set in constructor

            nb <- getWidget(obj)
            child <- getBlock(value)
            
            ## process ...
            theArgs <- list(...)

            ## label
            if(!is.null(theArgs$label)) {
              label <- theArgs$label
            } else if(!is.null(theArgs$name)) {
              label <- theArgs$name
            } else {
              label <- ""
            }
            if(!is.character(label))
              label = svalue(label)     # now a character
            
            index <- getWithDefault(theArgs$index, length(obj) + 1)
            index <- getWithDefault(theArgs$pageno, index)
            markup <- getWithDefault(theArgs$markup, FALSE)
            override.closebutton <-
              getWithDefault(theArgs$override.closebutton, FALSE)

            anchor <- getWithDefault(theArgs$anchor, c(0,0))
            expand <- getWithDefault(theArgs$expand, FALSE)

            ## closebutton
            doCloseButton <- FALSE
            if(!is.null(obj@closebuttons) &&
               as.logical(obj@closebuttons) &&
               !override.closebutton) {
              doCloseButton <- TRUE
            } 

            ## add drop motion for labels
            

            ## add at index
            nb$insertTab(index-1, child, label)
            

            ## record children, parent
            setParent(value, obj)
            addChild(obj, value)
          })
            
## Regular R methods treat gnotebook like a vector

## find out number of pages
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gNotebookQt"),
          function(x, toolkit) {
            nb <- getWidget(x)
            nb$count
          })

## return tabnames
setMethod(".names",signature(toolkit="guiWidgetsToolkitQt",x="gNotebookQt"),
          function(x, toolkit) {
            nb <- getWidget(x)
            n <- length(x)
            if(n > 0)
              vals <- sapply(1:n, function(i) nb$tabText(i-1))
            else
              vals <- character(0)
            return(vals)
          })

## can assigne with names(x) <-x or even names(x)[i] <- "single name"
setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x = "gNotebookQt"),
                 function(x,toolkit, value) {
                   nb <- getWidget(x)
                   n <- length(x)
                   
                   if(length(value) != n)
                     stop(gettext("New names for notebook must have proper length"))
                   
                   sapply(1:n, function(i) {
                     nb$setTabText(i-1, as.character(value[i]))
                   })

                   return(x)
                 })


## return widget contained in notebook page i as a  list or single widget
setMethod("[",
          signature(x="gNotebookQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gNotebookQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            nb <- getWidget(x)

            
            if(missing(i))
              i = seq_along(x)

            
            lst <- getChildren(x)[i]

            if(length(lst) == 0)          # can't get what isn't there
              return(NULL)
            
            if(length(i) == 1)
              return(lst[[1]])
            else
              return(lst)
          })


## Puts widget into a position
setReplaceMethod("[",
                 signature(x="gNotebookQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gNotebookQt"),
          function(x, toolkit, i, j, ..., value) {
            ##
            nb <- getWidget(x)

            if(missing(i))
              stop(gettext("Missing value for i"))

            theArgs <- list(...)
            label <- getWithDefault(theArgs$label, "")
            
            nb$insertTab(i[1], getBlock(value), label)

            ## record children, parent
            setParent(value, x)
            addChild(x, value)

            return(x)
          })


### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerexpose(obj,toolkit, handler,action)
          })


## h$pageno for tab
setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gNotebookQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(tab, h, ...) {
              h$tab <- h$pageno <- tab + 1 # 0-based
              handler(h)
            }
            ID <- .addhandler(obj, toolkit, "currentChanged", f, action, ...)
            return(ID)
          })

