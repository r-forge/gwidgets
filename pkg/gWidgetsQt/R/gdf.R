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


##################################################
##
## we only implement what isn't shared with gtable 
## TODO: 3rd mouse popup; addrow, add column, coerce types, write backend model an dput on light-weigth
## gwidgets spin

setClass("gDfQt",
         contains="gTableViewQt",
         prototype=prototype(new("gComponentQt"))
         )


## constructor for editing a data frame
setMethod(".gdf",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items = NULL,
                   name = deparse(substitute(items)),
                   do.subset = FALSE,
                   container=NULL,...)  {

            force(toolkit)


            
            ## XXX ???
            if(do.subset) {
              ## like filter, make a new widget
              obj <- .gdfWithSubset(toolkit,
                                   items,
                                   name,
                                   container,
                                   ...)
              return(obj)
            }
            


            
            tbl <- Qt$QTableWidget()

            ## customize widget for editing
            ## XXX equal row heights
            
            ## new object
            obj = new("gDfQt", block=tbl, widget=tbl,
              toolkit=toolkit, ID=getNewID(), e = new.env())

            tag(obj, "flags") <- c("selectable", "editable", "enabled")
            obj[,] <- items
            
            
            if(!is.null(container))
              add(container, obj, ...)


            return(obj)
            
          })


##
####################################################




## data frame methods
## get selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {
            tbl <- getWidget(obj)

            index <- getWithDefault(index, FALSE)
            drop <- getWithDefault(drop, TRUE)
            ## XXX This selection could be much more, here we grab the
            ## rectangle
            ind <- getTableWidgetSelection(tbl, as.rectangle=TRUE)
            
            if(is.null(ind))
              return(NA)
            if(index) {
              if(drop)
                return(ind$rows)
              else
                return(ind)
            }

            items <- obj[,]
            return(items[ind$rows, ind$columns, drop=drop])
          })
          
          
## set by index value selected value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   ## check
                   n <- dim(obj)[2]
                   if(n == 0)
                     return(obj)        # too small

                   tbl <- getWidget(obj)
                   
                   ## if value is a list, t has row and column, o/w
                   ## value is a vector
                   if(is.list(value)) {
                     setTableWidgetSelection(tbl, as.integer(value[[1]]),
                                             as.integer(value[[2]]))
                   } else {
                     ind <- try(as.integer(value), silent=TRUE)
                     if(inherits(ind, "try-error")) {
                       XXX("Can only specify indices")
                     } else {
                       setTableWidgetSelection(tbl, ind, columns=1:n)
                     }
                   }
                     

                   return(obj)
                 })


## [, [<- shared with gtable,

## names is colnames


## no dimnames for gGrid, only names
setMethod(".dimnames",
          signature(toolkit="guiWidgetsToolkitQt",x="gDfQt"),
          function(x,toolkit) {
            tbl <- getWidget(x)
            list(getTableWidgetRowNames(tbl), .names(x,toolkit))
          })
          

setReplaceMethod(".dimnames",
                 signature(toolkit="guiWidgetsToolkitQt",x="gDfQt"),
                 function(x, toolkit,  value) {
                   tbl <- getWidget(x)
                   
                   if(!is.list(value))
                     stop("value is a list with first element the row names, and second the column names")
                   rnames = make.names(value[[1]])
                   cnames = value[[2]]
                   d = dim(x)
                   if(is.null(rnames) || length(rnames) != d[1])
                     stop("Row names are the wrong size")
                   if(is.null(cnames) || length(cnames) != (d[2]))
                     stop("Column names are the wrong size")

                   ## set column names
                   names(x) <- cnames
                   setTableWidgetRowNames (tbl, rnames)
                          
                   return(x)
                 })

## We have a number of methods defined for gdf
## changed is cell changed
.addHandlergDfQt <- function(obj,  signal,  handler, action=NULL) {
  f <- function(row, column, h) {
    h$row <- row + 1
    h$column <- column + 1
    handler(h)
  }
  h <- list(obj=obj, action=action)
  ID <- qconnect(getWidget(obj), signal, f, user.data=h)
  invisible(ID)
}

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandlergDfQt(obj, "cellChanged",  handler, action)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandlergDfQt(obj, "cellClicked",  handler, action)
          })

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandlergDfQt(obj, "cellDoubleClicked", handler, action)
          })

.addColumnHandlergDfQt <- function(obj, signal, handler, action) {
  f <- function(index, h) {
    h$column <- index + 1
    handler(h)
  }
  w <- getWidget(obj)
  headerView <- w$horizontalHeader()
  h <- list(obj=obj, action=action)
  ID <- qconnect(headerView, signal, f, user.data=h)
  invisible(ID)
}
setMethod(".addhandlercolumnclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addColumnHandlergDfQt(obj, "sectionClicked", handler, action)
          })

setMethod(".addhandlercolumnrightclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            XXX("No binding for this event")
            return(NULL)
          })

setMethod(".addhandlercolumndoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gDfQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addColumnHandlergDfQt(obj, "sectionDoubleClicked", handler, action)
          })




##################################################
## Popup
## table for selecting values
## most methods in gdf.R inherited from gGrid class
setClass("gDfWithSubsetQt",
         contains="gDfQt",
         prototype=prototype(new("gDfQt"))
         )

.gdfWithSubset <- function(toolkit, items, name, container, ...) {

  ## add widget to a ggroup container
  g <- ggroup(horizontal=FALSE, cont=container, ...)
  tbl <- .gdf(toolkit, items, name=name, container=g, expand=TRUE)
  pg <- gexpandgroup("subset == ", cont=g, horizontal=TRUE)
  visible(pg) <- FALSE

  varSelector <- gcombobox(c("", names(tbl)), cont=pg)
  glabel("==", cont=pg)
  valSelector <- gcombobox(c(""), cont=pg)
  lbutton <- gbutton("<", cont=pg)
  rbutton <- gbutton(">", cont=pg)

  filterValues <- function() {
    var <- svalue(varSelector)
    val <- svalue(valSelector)
    if(var != "" && val != "") {
      ind <- as.character(tbl[,var, drop=TRUE]) == val
      visible(tbl) <- ind
    } else {
      visible(tbl) <- TRUE
    }
  }
    
  addHandlerClicked(varSelector, handler = function(h,...) {
    var <- svalue(varSelector)
    if(var != "") {
      values <- sort(unique(tbl[,var, drop=TRUE]))
      valSelector[] <- c("", values)
    } else {
      varSelector[] <- c("")
    }
    filterValues()
  })
  addHandlerClicked(valSelector, handler = function(h,...) {
    filterValues()
  })

  addHandlerClicked(lbutton, handler=function(h,...) {
    i <- svalue(valSelector, index=TRUE)
    if(i == 1)
      svalue(valSelector, index=TRUE) <- length(valSelector)
    else
      svalue(valSelector, index=TRUE) <- i - 1
    filterValues()
  })                    

  addHandlerClicked(rbutton, handler=function(h,...) {
    i <- svalue(valSelector, index=TRUE)
    if(i == length(valSelector))
      svalue(valSelector, index=TRUE) <- 1
    else
      svalue(valSelector, index=TRUE) <- i + 1
    filterValues()    
  })                    

  obj <- new("gDfWithSubsetQt",
             block=g, widget=getWidget(tbl),
             toolkit=toolkit, ID=tbl@ID, e=tbl@e)
  return(obj)

}

