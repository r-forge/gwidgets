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


## use GridView to show a table

## working

## svalue
## transport
## click and double click handler
## icon fun: use class(icon) to mark
## multiple is  working
## [<- : needs to have data frame with same column types, no. of columns

## get working:
## names<-, names for headers


##' A table widget
##'
##' A widget for displaying a data frame in tabular format
##' The \code{[<-} method is only for replacing the data frame and must have the same class columns as the original. One can load the inital data frame with a 0-row data frame
##' @param items items (data frame) to display. Can be modified with
##' \code{[<-} method. Changing column types is not supported.
##' @param multiple logical. Do we allow multiple selection
##' @param chosencol The svalue() method returns a single value, by default. This species column of that value.
##' @param icon.FUN A function to generate icons given by items. Icons should return urls, as does getStockIcons()
##' @param filter.column Ignored in gWidgetsWWW
##' @param filter.labels Ignored in gWidgetsWWW
##' @param filter.FUN Ignored in gWidgetsWWW. Implement filtering manually. The visible<- method is automatically available (some toolkits need \code{filter.FUN="manual"}). Also there is a \code{$filter} proto method for filtering by a regular expression. Sorting is enabled through the headers
##' @param handler single click handlers
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to parent container's \code{add} method
##' @note The default size of the widget is lacking. The \code{size<-}
##' method is often needed for proper layout. No \code{names} or
##' \code{names<-} method.
##' @export
gtable <- function(items, multiple = FALSE, chosencol = 1,
                   icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL,
                   filter.FUN = NULL, handler = NULL, action = NULL,
                   container = NULL, ...,
                   width=200, height=200
                   ) {

  widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                                      ..multiple = multiple,
                                      ..icon.FUN = icon.FUN,
                                      ..width=width, ..height=height
                                      )

  class(widget) <- c("gTable",class(widget))

  theArgs <- list(...)
  
  ## set up store
  store <- EXTStore$new(toplevel=container$toplevel)
  store$ID <- container$newID()       # set ID

  ## load in items
  if(!is.data.frame(items)) {
    items <- data.frame(items, stringsAsFactors=FALSE)
  }
  
  store$chosenCol <- chosencol
  if(!is.null(icon.FUN)) {
    n <- length(items)
    icons <- icon.FUN(items)
    class(icons) <- c("icon",class(icons)) # to render as icon
    items$..icons <- icons
    items <- items[,c(n+1,1:n)]
    ## must up the chosen col by 1
    store$chosenCol <- store$chosenCol + 1
  }
  items <- cbind("__index"=seq_len(nrow(items)), items)
  store$data <- items
  widget$..store <- store

  ## set up widget
  widget$setValue(value = 1)            # first column is selected on startup
  
  

  ## setValues need to add in icons.
  widget$setValues <- function(.,i,j,...,value) {
    ## XXX need to include i,j stuff
    ## XXX value must be a data frame of the same size as original
    ## add in icons if present
    items <- value
    if(exists("..icon.FUN", envir=., inherits=FALSE)) {
      if(!is.null(.$..icon.FUN)) {        # adjust icons
        icon.FUN <- get("..icon.FUN",.)
        icons <- icon.FUN(items)
        class(icons) <- c("icon",class(icons)) # to render as icon
        n <- length(items)
        items$..icons <- icons
        items <- items[,c(n+1,1:n)]
      }
    }
    items <- cbind("__index"=seq_len(nrow(items)), items)
    .$..store$data <- items

    if(exists("..shown",envir=., inherits=FALSE))
      ##cat(.$setValuesJS(...), file=stdout())
      .$addJSQueue(.$setValuesJS(...))
  }

 

  ## transport mouse clicks back as row indices. Can be multiple or single
  widget$transportSignal <- c("cellclick")
  widget$ExtConstructor <- "Ext.grid.GridPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(store = String(.$..store$asCharacter()),
                columns = String(.$makeColumnModel()),
                stripeRows = TRUE,
                enableRowBody = TRUE, 
                frame = FALSE,
                autoExpandColumn=tail(names(.$..store$data), n=1)
                ) ## also autoExpandColumn, XXX
    
    if(.$..multiple) {
      out[["sm"]] <- String() +
        'new Ext.grid.RowSelectionModel({singleSelect:false})'
    } else {
      out[["sm"]] <- String() +
        'new Ext.grid.RowSelectionModel({singleSelect:true})'
    }

    ## size in panel config, not setStyle
    if(exists("..width",envir = .,inherits=FALSE))
      out[["width"]] <- .$..width
    else
      out[["width"]] <- "auto"
    
    if(exists("..height",envir = .,inherits=FALSE))
      out[["height"]] <- .$..height
    else
        out[["height"]] <- "auto"
    
    return(out)
  }

  widget$footer <- function(.) {
    sprintf('%s.getSelectionModel().selectFirstRow();',.$asCharacter())
  }
  
  ## changed = clicked
  widget$addHandlerClicked <- function(.,handler, action=NULL, ...) {

    ## we need to set up some stuff
    .$addHandler(signal="cellclick",
                 handler = handler,
                 action = action,
                 handlerArguments = "grid, rowIndex, colIndex, e",
                 handlerValue = "var value = rowIndex + 1;"
                 )
  }

  ## double click is default
  widget$addHandlerDoubleclick <- widget$addHandlerChanged <- function(.,handler, action=NULL, ...) {
    ## we need to set up some stuff
    .$addHandler(signal="dblclick",
                 handler = handler,
                 action = action,
                 handlerArguments = "grid, rowIndex, colIndex, e",
                 handlerValue = "var value = rowIndex + 1;")
  }
  
  
  ###
  container$add(widget,...)

  if(!is.null(handler))
    widget$addHandlerChanged(handler, action=action)
  
  
  invisible(widget)
  
}
