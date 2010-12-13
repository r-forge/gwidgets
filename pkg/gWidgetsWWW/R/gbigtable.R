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

##' Show big data tables using paging features of ext
##'
##' @param items data frame to display
##' @param multiple logical can select one or more
##' @param chosencol for svalue method when \code{index=FALSE}
##' @param pageSize number of items to display per page
##' @param handler double click handler
##' @param action passed to handler
##' @param container standard container object
##' @param ... passed to add method
##' @param width integer pixel width. A grid object must have width and height
##' @param height integer pixel height
##' @return a widget instance
##' @examples
##' \dontrun{
##' require(MASS, quietly=TRUE)
##' w <- gwindow("test")
##' g <- ggroup(horizontal=FALSE, cont=w)
##' tbl <- gbigtable(Aids2, cont=g)
##' addHandlerDoubleclick(tbl, handler=function(h,...) {
##'   gmessage(svalue(h$obj), parent=w)
##' })
##' size(tbl) <- list(width=800,height=400, columnWidths=c(100))
##' visible(w) <- TRUE
##' }
## @seealso \code{\link{gtable}}
gbigtable <- function(items, multiple = FALSE, chosencol = 1,
                      pageSize = 25, handler = NULL, action = NULL,
                      container = NULL, ...,
                      width=200, height=200
                      ) {
  
  widget <- EXTComponentWithProxyStore$new(toplevel=container$toplevel,
                                           ..multiple = multiple,
                                           ..width=width, ..height=height
                                           )

  class(widget) <- c("gTable",class(widget))

  theArgs <- list(...)
  
  ## set up store
  store <- EXTProxyStore$new(toplevel=container$toplevel, pageSize=pageSize)
  store$ID <- container$newID()       # set ID
  container$toplevel$addStore(store)
  store$chosenCol <- chosencol

  
  ## load in items
  if(!is.data.frame(items)) {
    items <- data.frame(items, stringsAsFactors=FALSE)
  }
  
  items <- cbind("__index"=seq_len(nrow(items)), items)
  store$data <- items
  widget$..store <- store

  ## set up widget
  widget$setValue(value = 1)            # first column is selected on startup

  
  widget$setValues <- function(.,i,j,...,value) {
    ## XXX need to include i,j stuff
    ## XXX value must be a data frame of the same size as original
    ## add in icons if present
    items <- value
    items <- cbind("__index"=seq_len(nrow(items)), items)
    .$..store$data <- items

    if(exists("..shown",envir=., inherits=FALSE))
      .$addJSQueue(.$setValuesJS(...))
  }

  widget$setValuesJS <- function(., ...) {
    ## get browser to reload itself
    out <- String() +
      sprintf("%s.getTotalCount = function() {return %s};", .$..store$asCharacter(), nrow(.$..store$data)) +
        sprintf("%s.load({params:{start:0, limit:%s}});",
                .$..store$asCharacter(), .$..store$pageSize)
    .$addJSQueue(out)
  }
  
  ##' visibility
  widget$setVisible <- function(., value) {
    ## XXX nothing to do here, can't find the visible (setHidden? method we need)
    ## use $filter instead.
  }


  widget$ExtConstructor <- "Ext.grid.GridPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(store = String(.$..store$asCharacter()),
                columns = String(.$makeColumnModel()),
                stripeRows = TRUE,
                enableRowBody = TRUE, 
                frame = FALSE
                ,autoExpandColumn=tail(names(.$..store$data), n=1)
                ) ## also autoExpandColumn, XXX
    

    out[['bbar']] = String() +
      paste("new Ext.PagingToolbar({",
            sprintf("pageSize: %s,",.$..store$pageSize),
            (String("store:") + .$..store$asCharacter() + ','),
            "displayInfo: true,",
            "displayMsg: 'Displaying topics {0} - {1} of {2}',",
            "emptyMsg: 'No topics to display',",
#            "items:[",
#            "'-', {",
#            "pressed: true,",
#            ## XXX still need to get rid of button for preview.
#            "enableToggle:true,",
#            "text: 'Show Preview',",
#            "cls: 'x-btn-text-icon details',",
#            "toggleHandler: function(btn, pressed){",
#            (String("var view =") + .$asCharacter() + ".getView();"),
#            "view.showPreview = pressed;",
#            "view.refresh();",
#            "}",
#          "}]",
          "})",
            collapse="\n")

    
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


  ## select first row
  widget$footer <- function(.) {
    sprintf("%s.getSelectionModel().selectFirstRow();\n", .$asCharacter())
  }
  
  ## ## changed = double clicked
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
