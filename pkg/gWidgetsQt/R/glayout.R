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
            gridlyt$setContentsMargins(1,1,1,1) # area around widget
            
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
            anchor <- getWithDefault(theArgs$anchor, NULL)
            expand <- getWithDefault(theArgs$expand, FALSE)
            fill <- getWithDefault(theArgs$fill, NULL)

            if(!is.null(fill) || !is.null(anchor))
              expand <- TRUE
            
            parent <- getWidget(x)
            child <- getBlock(value)

            ## add -- depends on object
            if(is(child,"QWidget")) {
              ## We have expand/anchor/fill 

              align <- Qt$Qt$AlignTop | Qt$Qt$AlignLeft # default alignment
              ## anchor or fill, share with ggroup?
              if(is.null(anchor)) {
                if(!is.null(fill)) {
                  if(fill == "x")
                    child$setSizePolicy(Qt$QSizePolicy$Fixed, # Preferred? MinimumExpanding?
                                        Qt$QSizePolicy$Expanding)
                  else if(fill == "y")
                    child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                        Qt$QSizePolicy$Fixed)
                  else                  # default is fill = "both" when no anchor, but expand
                    child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                        Qt$QSizePolicy$Expanding)
                }
              } else {
                ## anchor non NULL
                align <- xyToAlign(anchor)
                ## halign <- list(Qt$Qt$AlignLeft, Qt$Qt$AlignHCenter, Qt$Qt$AlignRight)
                ## valign <- list(Qt$Qt$AlignBottom, Qt$Qt$AlignVCenter, Qt$Qt$AlignTop)
                ## if(is.numeric(anchor) && length(anchor) >= 2) {
                ##   align <- halign[[anchor[1] + 2]] | valign[[anchor[2] + 2]]
                ## }
              }
              
              if(homogeneous || expand) {
                ## stretch rows
                stretch <- 1
                for(col in seq(min(i), min(i) + length(i) - 1))
                  parent$setColumnStretch(col-1, stretch)
                for(row in seq(min(j), min(j) + length(j) - 1))
                  parent$setRowStretch(row-1, stretch)
              }

              parent$addWidget(child, min(i)-1, min(j)-1, length(i), length(j), align)
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

