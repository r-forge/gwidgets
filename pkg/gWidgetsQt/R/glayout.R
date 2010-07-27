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

setClass("gLayoutQt",
         contains="gContainerQt",
         prototype=prototype(new("gContainerQt"))
         )

## an gWidget for tables
 
setMethod(".glayout",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   homogeneous = FALSE,
                   spacing = 10,        # amount (pixels) between row, cols, NULL=1
                   container = NULL, ...
                   ) {
            
            force(toolkit)

            w <- Qt$QWidget()
            gridlyt <- Qt$QGridLayout()
            w$setLayout(gridlyt)        # layout must be set *before* children added

            if(is.null(spacing))
              spacing <- 1L
            gridlyt$setSpacing(as.integer(spacing)) # does both horizontal and vertical
            ## gridlyt$setVerticalSpacing(spacing)
            ## gridlyt$setHorizontalSpacing(spacing)
            gridlyt$setContentsMargin(1,1,1,1) # area around widget
            
            obj = new("gLayoutQt",
              block=w, widget=gridlyt,
              toolkit=toolkit, e = new.env(), ID=getNewID())
            tag(obj, "childlist") <- list()
            
            if(!is.null(container))
              add(container, obj, ...)


            ## how to add in per column adjustments?
            adjust = "center"                             # left or right or center

            tag(obj,"homogeneous") <- homogeneous
            tag(obj,"spacing") <- as.numeric(spacing)
            tag(obj,"adjust") <- adjust
            
            invisible(obj)
          })


## for adding
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gLayoutQt",
                    value="gWidgetQt"),
          function(obj, toolkit, value, ...) {
            ## this is a stub so that we can do
            ## tbl[i,j] <- glabel(i, cont=tbl)
          })

## retrieve values
setMethod("[",
          signature(x="gLayoutQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gLayoutQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            l <- tag(x, "childlist")
            ind <- sapply(l, function(comp) {
              i[1] %in% comp$x && j[1] %in% comp$y
            })
            if(any(ind))
              return(l[ind][[1]]$child) # first
            else
              NA
          })


## how we populate the table
setReplaceMethod("[",
                 signature(x="gLayoutQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gLayoutQt"),
          function(x, toolkit, i, j, ..., value) {

            if(is.character(value)) {
              value <- glabel(value, cont = x)
            }
            
            ## What to do with these?
            theArgs = list(...)
            spacing <- tag(x,"spacing")
            homogeneous <- as.logical(tag(x,"homogeneous"))
            anchor <- getWithDefault(theArgs$spacing, c(0,0))
            expand <- getWithDefault(theArgs$expand, FALSE)
            fill <- getWithDefault(theArgs$fill, NULL)



            parent <- getWidget(x)
            child <- getBlock(value)

            ## add -- depends on object
            if(is(child,"QWidget")) {
              ## We have expand/anchor/fill implement for  ggroup (actually .add) so we
              ## use ggroup here, but it is an issue -- there is too much spacing
              ## even with everything cranked down to 0.
              ## fill/anchor only applies if expand is TRUE, so we check.            
              if(expand) {
                tmp <- .ggroup(toolkit, expand=TRUE, fill="both", spacing=0)
                add(tmp, value, expand=expand, fill=fill, anchor=anchor)
                child <- getBlock(tmp)
              }

              ## homogeneous is governed by columnstretch, but not sure what this should be
              if(homogeneous) {
                stretch <- 1
              } else {
                stretch <- 0
              }
              for(col in seq(min(i), min(i) + length(i) - 1))
                  parent$setColumnStretch(col-1, stretch)
              
              parent$addWidget(child, min(i)-1, min(j)-1, length(i), length(j))
              child$show()
            } else if(is(child, "QLayout")) {
              parent$addLayout(child, min(i)-1, min(j)-1, length(i), length(j))
            }
            
            ## record children, parent
            setParent(value, x)
            addChild(x, value)

            ## add to list so [ method works
            l <- tag(x, "childlist")
            l[[as.character(length(l) + 1)]] <- list(x=i, y=j, child=value)
            tag(x, "childlist") <- l
            
            
            return(x)
          })

