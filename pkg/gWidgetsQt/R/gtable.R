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

##XXX df[,] = 1 error


## drag  interface. Taken from gvarbrowser





setClass("gTableQt",
         contains="gTableViewQt",
         prototype=prototype(new("gTableViewQt"))
         )



## ## constructor for selecting values from a data set -- not meant for editing
setMethod(".gtable",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {

            ## NOT IMPLEMENTED
            ## * sorting

            force(toolkit)

            
            ## setup widget
            tbl <- Qt$QTableWidget()

            ## customize widget
            ## no rows names
            tbl$verticalHeader()$setVisible(FALSE)
            ## alternate shading
            tbl$setAlternatingRowColors(TRUE)
            ## stretch last section
            header <- tbl$horizontalHeader()
            header$setStretchLastSection(TRUE)

            ## select entire row to mathc other toolkits
            ## XXX force in !multiple -- just a hack though
            qconnect(tbl, "cellClicked", function(row, column, l) {
              tbl <- l$tbl; multiple <- l$multiple
              if(!l$multiple) {
                m <- tbl$selectionModel()
                m$clearSelection()
              }
              setTableWidgetSelection(tbl, rows=row+1, full.row=TRUE)
            }, user.data=list(tbl=tbl, multiple=multiple))
            

            ## if filtering we call a different constructor
            ## we are filtering if filter.FUN or filter.column is
            ## not null *UNLESS* filter.FUN = "manual"
            if(!is.null(filter.column) || !is.null(filter.labels) ||
               (!is.null(filter.FUN) && !is.character(filter.FUN))
                ) {
              obj <- .gtableWithFilter(toolkit,
                                       items=items,
                                       multiple = multiple,
                                       chosencol = 1,                        # for drag and drop, value
                                       icon.FUN = icon.FUN,
                                       filter.column = filter.column,
                                       filter.labels = filter.labels,
                                       filter.FUN = filter.FUN,   # two args gtable instance, filter.labels element
                                       handler = handler,
                                       action = action,
                                       container =container,
                                       ...)
              
              return(obj)
            }
            
            
            obj <- new("gTableQt",block=tbl, widget=tbl,
              toolkit=toolkit,ID=getNewID(), e = new.env())
            
            tag(obj, "chosencol") <- chosencol
            tag(obj,"icon.FUN") <- icon.FUN

            
            ## populate, set names, ...
            obj[] <- items
            visible(obj) <- NULL        # after setting values

            ## add handler
            if (!is.null(handler)) 
              tag(obj, "handler.id") <-  addhandlerchanged(obj,handler,action)

            
            ## add to container
            if(!is.null(container))
              add(container, obj,...)
            
            return(obj)
            
          })


## incorporate chosenval here
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tbl <- getWidget(obj)
            chosencol <- getWithDefault(tag(obj,"chosencol"), 1)

            index <- getWithDefault(index, FALSE)
            drop <- getWithDefault(drop, TRUE)

            
            ## get selected rows indices
            ## ind is right, no need to account for visible
            ind <- getTableWidgetSelection(tbl, as.rectangle=TRUE)


            if(is.null(ind))
              return(ind)

            if(index) {
              if(drop) {
                ## index=TRUE, drop=TRUE
                return(ind$rows)
              } else {
                ## index=TRUE, drop=FALSE
                return(ind)
              }
            } else {
              if(drop)
                return(obj[ind$rows, chosencol, drop=drop])
              else
                return(obj[ind$rows, , drop=drop])
            }
          })

## set the selected value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   d <- dim(obj)
                   if(d[1] == 0 || d[2] == 0)
                     return()           # too small
                   
                   tbl <- getWidget(obj)
                   index <- getWithDefault(index, FALSE)
                   chosencol <- getWithDefault(tag(obj,"chosencol"), 1)
                   
                   if(index) {
                     if(is.list(value))
                       setTableWidgetSelection(tbl, rows=value[[1]], columns=value[[2]], full.row=FALSE)
                     else
                       setTableWidgetSelection(tbl, rows=value, full.row=TRUE)
                   } else {
                     items <- obj[,chosencol, drop=TRUE]
                     if(value %in% items) {
                       ind <- which(items == value)
                       setTableWidgetSelection(tbl, list(rows=ind, columns=1:d[2]))
                     }
                   }
                   return(obj)
                 })


## retrieve values
setMethod("[",
          signature(x="gTableViewQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gTableViewQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            tbl <- getWidget(x)
            colClasses <- tag(x, "colClasses")
            items <- getValueFromTableWidget(tbl, colClasses)
            d <- dim(items)
            if(d[2] == 0)
              return(c())
            if(missing(j))
              j <- 1:d[2]

            if(d[1] == 0)
              return(items[0,j, drop=drop])

            if(missing(i))
              i <- 1:d[1]
            
            return(items[i,j, drop=drop])
          })
            

## XXX -- harder one
## do [,]; [i,], [,j] (no new row, column); [i,j] no new value
## replace values
setReplaceMethod("[",
                 signature(x="gTableViewQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gTableViewQt"),
          function(x, toolkit, i, j, ..., value) {

            tbl <- getWidget(x)

            ## we can pass in flags here
            flags <- tag(x, "flags")
            if(is.null(flags)) {
              theArgs <- list(...)
              flags <- theArgs$flags
            }
            if(is.null(flags))
              flags <- c("selectable", "enabled")

            
            colClasses <- tag(x, "colClasses")
            ## XXX all wrong
            ## 1) get values (if i or j)
            ## 2) merge in value
            ## 3) redraw, visible, icons

            if(missing(i) && missing(j)) {
              
              ## we want a data frame for value
              if(missing(value))
                value <- data.frame(x=c(""),stringsAsFactors=FALSE)
              ## coerce value to a data frame
              ## vector, not matrix
              if(!is(value,"matrix") && !is(value,"data.frame")) {
                value <- data.frame(value=value, stringsAsFactors=FALSE)
              }
              ## matrix
              if(is.matrix(value)) {
                value <- data.frame(value, stringsAsFactors=FALSE)
              }
              
              ## value is a data frame. If first column named visible and logical,
              ## and second named icon and character, make an assumption
              if(ncol(value) > 2 &&
                 (names(value)[1] == "visible" && is.logical(value[,1])) &&
                 (names(value)[2] == "icon" && is.character(value[,2]))) {
                visibleRows <- value[,1]
                icons <- value[,2]
                value <- value[,-(1:2)]
              } else {
                visibleRows <- getWithDefault(tag(x, "visible"), seq_len(nrow(value)))
                if(!is.null(tag(x, "icon.FUN"))) {
                  icons <- tag(x, "icon.FUN")(value)
                } else {
                  icons <- NULL
                }
              }
              ## now set data frame
              setTableWidgetFromDataFrame(tbl, value, flags=flags)
              ## visible. (Could use visible<- here
              sapply(seq_along(visibleRows), function(i) {
                tbl$setRowHidden(i-1, !visibleRows[i])
              })
              ## icons
              setTableWidgetIcons(tbl, icons)
              ## save colClasses
              colClasses <- sapply(value, function(i) head(class(i), n=1))
              tag(x, "colClasses") <- colClasses

            } else {
              if(missing(j))
                j <- 1:dim(x)[2]
              if(missing(i))
                i <- 1:dim(x)[1]


              ## coerce value to a data frame
              if(is.matrix(value))
                value <- data.frame(value, stringsAsFactors=FALSE)
              if(is.vector(value))
                value <- data.frame(value, stringsAsFactors=FALSE)
              if(!is.data.frame(value))
                value <- data.frame(value, stringsAsFactors=FALSE)
              
              if((length(i) != dim(value)[1]) &&
                 (length(j) != dim(value)[2])) {
                warning(gettext("Indices and value do match in size."))
                return()
              }

              if((max(i) > tbl$rowCount) ||
                 (max(j) > tbl$columnCount)) {
                warning(gettext("Can't extend size of table this way. Respecify using obj[,] <- dataframe"))
                return()
              }

              ## coerce to data frame
              if(is.null(dim(value)))
                value <- data.frame(items=value, stringsAsFactors=FALSE)
              if(!is.data.frame(value))
                value <- data.frame(value, stringsAsFactors=FALSE)
              
              ## set column by column
              for(col in seq_along(length(j))) {
                for(row in seq_along(i)) {
                  setTableWidgetCell(tbl, value[row, col], row, col, flags=flags)
                }
                ## format column
                val <- do.call(sprintf("as.%s", colClasses[j[col]]), list(value[,j]))
                formatColumn(val, j[col], tbl)
              }

              ## if 1 in j, do icons
              if(1 %in% j) {
                icon.FUN <- tag(x, "icon.FUN")
                if(!is.null(icon.FUN)) {
                  icons <- icon.FUN(x[,])
                  setTableWidgetIcons(tbl, icons)
                }
              }
              ## visibility
              
            }
              
            
            return(x)
          })


## dim
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkitQt",x="gTableViewQt"),
          function(x, toolkit) {
            tbl <- getWidget(x)
            c(tbl$rowCount, tbl$columnCount)
          })
## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gTableViewQt"),
          function(x, toolkit) {
            dim(x)[2]
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTableViewQt"),
          function(obj, toolkit, set=TRUE, ...) {
            visible <- tag(obj,"visible")
            if(is.null(visible))
              visible <- rep(TRUE, dim(obj)[1])
            return(visible)
          })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gTableViewQt"),
                 function(obj, toolkit, ..., value) {
                   d <- dim(obj)
                   if(d[1] == 0)
                     return(obj)
                   
                   value <- rep(value, length=d[1]) # recycle!
                   tag(obj,"visible") <- value
                   
                   ## now redraw
                   tbl <- getWidget(obj)
                   sapply(seq_along(value), function(i) tbl$setRowHidden(i-1, !value[i]))

                   return(obj)
                 })

setMethod(".names",
          signature(toolkit="guiWidgetsToolkitQt",x="gTableViewQt"),
          function(x, toolkit) {
            tbl <- getWidget(x)
            getTableWidgetNames(tbl)
          })

setReplaceMethod(".names",
                 signature(x="gTableViewQt"),
                 function(x,toolkit, value) {
                   tbl <- getWidget(x)
                   d <- dim(x)
                   if(d[2] == 0) {
                     return(x)
                   } else {
                     setTableWidgetNames(tbl, value)
                   }
                   return(x)
                 })

## ## width setting is hacked in, if value has 1 or more than 2 values, we assume
## ## they are column widths
## setReplaceMethod(".size", 
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
##                  function(obj, toolkit, ..., value) {

##                    ## width is tricky. Use current widths
##                    d <- dim(obj); m <- d[1]; n <- d[2]
##                    widths <- sapply(1:n, function(j) {
##                      tclvalue(tcl(getWidget(obj), "column", j-1, "-width"))
##                    })
##                    widths <- as.numeric(widths)
                   
##                    curWidth <- sum(widths)
##                    widths <- floor((1+widths) * value[1]/curWidth)

##                    ## set width
##                    sapply(1:n, function(j) {
##                      tcl(getWidget(obj), "column", j-1, width=widths[j])
##                    })
                   
##                    ## set height
##                    height=value[2]
##                    tkconfigure(getWidget(obj), height = floor(height/16))

##                    return(obj)
##                  })


## handlers
## changed is double click
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerdoubleclick(obj, toolkit, handler, action, ...)
          })


## when a cell is double clicked
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            w <- getWidget(obj)
            f <- function(row, column) {
              h <- list(obj=obj, action=action)
              h$row <- row
              h$column <- column
              handler(h)
            }
            id <- qconnect(w,"cellDoubleClicked", f)
            invisible(id)
          })

## when a cell is clicked
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            w <- getWidget(obj)
            f <- function(row, column, h) {
              h$row <- row
              h$column <- column
              handler(h)
            }
            id <- qconnect(w,"cellClicked", f, user.data=list(obj=obj,action=action))
            invisible(id)
          })


##################################################
##################################################
### for filtering


## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gTableWithFilterQt",
         contains="gTableQt",
         prototype=prototype(new("gTableQt"))
         )


setGeneric(".gtableWithFilter",
           function(toolkit,
                    items,
                    multiple = FALSE,
                    chosencol = 1,                        # for drag and drop, value
                    icon.FUN = NULL,
                    filter.column = NULL,
                    filter.labels = NULL,
                    filter.FUN = NULL,   # two args gtable instance, filter.labels element
                    handler = NULL,
                    action = NULL,
                    container = NULL,
                    ...)
           standardGeneric(".gtableWithFilter")
           )

setMethod(".gtableWithFilter",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items,
                   multiple = FALSE,
                   chosencol = 1,                        # for drag and drop, value
                   icon.FUN = NULL,
                   filter.column = NULL,
                   filter.labels = NULL,
                   filter.FUN = NULL,   # two args gtable instance, filter.labels element
                   handler = NULL,
                   action = NULL,
                   container = NULL,
                   ...) {
            
            ## we only get here *if* we are filtering
 

            g <- .ggroup(toolkit, horizontal=FALSE, container=container, ...)

            fg <- ggroup(cont=g)
            filterByLabel <- glabel("Filter by:", container=fg)
            filterPopup <- gcombobox(c(""), container=fg)
            addSpring(fg)
            
            tbl = .gtable(toolkit,
              items,
              multiple=multiple,
              chosencol=chosencol,
              cont=g, expand=TRUE)

            

            
            ## make an object to return
            obj <- new("gTableWithFilterQt", block=getBlock(g),widget=getWidget(tbl), 
              toolkit=toolkit,ID=getNewID())

            tag(obj, "filterPopup") <- filterPopup
            tag(obj, "filterByLabel") <- filterByLabel
            tag(obj, "colClasses") <- tag(tbl, "colClasses")
            
            ## one of filter.column or filter.fun is non-NULL
            if(is.null(filter.FUN)) {
              ## define filter.FUN
              filter.FUN <- function(DF, filterBy) {
                if(filterBy == "") return(rep(TRUE,nrow(DF)))
                inds <- as.character(DF[,filter.column]) == filterBy
              }
              
              ## set up droplist
              filterPopup[] <- c("",sort(unique(as.character(items[,filter.column]))))
              svalue(filterByLabel) <- paste("Filter by",names(items)[filter.column],"==",sep=" ", collapse=" ")
            } else if(filter.FUN != "manual") {
              ## set up droplist
              filterPopup[] <- c("",filter.labels)
            }

            tag(obj,"filter.FUN") <- filter.FUN

            ## get obj from scoping
            addHandlerChanged(filterPopup,action=tbl,
                              handler=function(h,...) {
                                tbl <- h$action
                                DF <- tbl[,]
                                fval <- svalue(h$obj) # popup
                                if(is.null(fval) || fval=="") {
                                  visible(tbl) <- TRUE
                                } else {
                                  filter.fun <- tag(tbl,"filter.FUN")
                                  inds <- filter.FUN(DF, fval)
                                  ## update  tbl
                                  visible(tbl) <- inds
                                }
                              })
            ## add handler to gtable object, but pass in override for methods
            if(!is.null(handler)) 
             tag(obj, "handler.id") <- addhandlerchanged(tbl,handler,action,actualobj=obj,...)
            
            return(obj)
          })


          

## setMethod(".svalue",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gTableWithFilterQt"),
##           function(obj, toolkit, index=NULL, drop=NULL,...) {

##             if(!is.null(index) && index) {
##               gwCat("The index refers to the visible data value, not the entire data frame\n")
##             }

##             return(svalue(obj@widget, toolkit=toolkit, index=index, drop=drop, ...))

##           })

## ## refers to visible
## setReplaceMethod(".svalue",
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gTableWithFilterQt"),
##                  function(obj, toolkit, index=NULL, ..., value) {
##                    svalue(tbl, toolkit=toolkit, index=index,  ...) <- value

##                    return(obj)
##                  })


## ## retrieve values
## setMethod(".leftBracket",
##           signature(toolkit="guiWidgetsToolkitQt",x="gTableWithFilterQt"),
##           function(x, toolkit, i, j, ..., drop=TRUE) {
##             tbl = tag(x,"tbl")
##                                         # dot function
##             .leftBracket(tbl, toolkit, i, j, ..., drop=drop)
##           })
            
## setMethod("[",
##           signature(x="gTableWithFilterQt"),
##           function(x, i, j, ..., drop=TRUE) {
##             .leftBracket(x, x@toolkit, i, j, ..., drop=drop) 
##           })
## ## replace values
## setReplaceMethod(".leftBracket",
##           signature(toolkit="guiWidgetsToolkitQt",x="gTableWithFilterQt"),
##           function(x, toolkit, i, j, ..., value) {
##             if(!missing(i) || !missing(j)) {
##               gwCat(gettext("[<- only replaces the entire object. Try obj[,]<-value\n"))
##               return(x)
##             }

##             ## underlying gtable object
##             tbl = tag(x,"tbl")

##             ## We have to a) update allItems, b) update table
##             tag(x, "allItems") <- value
##             ## tbl needs to be filtered
##             DF = value
##             fval = svalue(tag(x, "filterPopup"))
##             if(fval == "") {
##               tbl[,] <- DF
##             } else {
##               filter.FUN = tag(x,"filter.FUN")
##               inds = filter.FUN(DF, fval)
##               tbl[,] <- DF[inds,,drop=FALSE]
##             }
              

##             return(x)
##            })

## setReplaceMethod("[",
##                  signature(x="gTableWithFilterQt"),
##                  function(x, i, j,..., value) {
##                    .leftBracket(x, x@toolkit, i, j, ...) <- value
##                    return(x)
##                  })

## ## dim
## setMethod(".dim",
##           signature(toolkit="guiWidgetsToolkitQt",x="gTableWithFilterQt"),
##           function(x, toolkit) {
##             tbl = getWidget(x)
##             return(c(tbl$rowCount,tbl$columnCount))
##           })
## ## length
## setMethod(".length",
##           signature(toolkit="guiWidgetsToolkitQt",x="gTableWithFilterQt"),
##           function(x, toolkit) {
##             dim(x)[2]
##           })

## ## size<- work on tr
## setReplaceMethod(".size", 
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gTableWithFilterQt"),
##                  function(obj, toolkit, ..., value) {
##                    tbl = tag(obj,"tbl")
##                    size(tbl) <- value
##                    return(obj)
##                  })

## ## handlers

## setMethod(".addhandlerchanged",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gTableWithFilterQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             tbl = tag(obj,"tbl")
##             .addhandlerdoubleclick(tbl, toolkit, handler, action,actualobj=obj)
##           })


## ## when a selection is changed
## setMethod(".addhandlerclicked",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gTableWithFilterQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             tbl = tag(obj,"tbl")
##             .addHandler(tbl,toolkit,signal="selectionChanged", handler, action,
##                         actualobj=obj)
##           })


         
         
         

