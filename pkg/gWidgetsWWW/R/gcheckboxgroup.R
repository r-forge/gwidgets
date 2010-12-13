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

##'
##' @param items vector of items to select from
##' @param checked initial value of checked state. Recycled
##' @param horizontal Layout horizontally?
##' @param use.table If TRUE, uses a grid widget with checkboxes to
##' display. If TRUE, horizontal is ignored, and items may be a data
##' frame.
##' @param handler handler called when state changes
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of container
##' @export
gcheckboxgroup = function (items, checked = FALSE, horizontal = FALSE, use.table=FALSE,
  handler = NULL, action = NULL,
  container = NULL, ...) {

  ## use.table?
  if(use.table) {
    out <- gcheckboxgrouptable(items, checked=checked, handler=handler, action=action, container=container, ...)
    return(out)
  }

  
  ## use a checkbox if only one item
  if(length(items) == 1) {
    out <- gcheckbox(items, checked = checked, handler=handler, action=action, container = container, ...)
    return(out)
  }

  widget <- EXTComponentWithItems$new(toplevel=container$toplevel,
                                      ..checked = checked,
                                      ..horizontal = horizontal,
                                      ..handler = handler,
                                      ..action = action
                                      )
  class(widget) <- c("gCheckboxgroup",class(widget))

  

  widget$assignValue <- function(., value) {
    value <- value$value     # a list
    .$..data <- as.logical(value)
  }


  widget$setValue <- function(., index=NULL,..., value) {
    ## values can be set by index, logical, or names

    n <- length(.); items <- .$getValues()
    if(is.character(value)) {
      value <- sapply(items, function(i) i %in% value)
    } else if(is.numeric(value) || (!is.null(index) && index)) {
      value <- sapply(1:n, function(i) i %in% value)
    } else if(!is.logical(value)) {
      ## error
      cat("Value should be logical vector, vector of indices, or character vector of names\n")
    }

    .$..data <- rep(value, length=n)

    if(exists("..shown",envir=., inherits=FALSE))
      ##cat(.$setValueJS())
      .$addJSQueue(.$setValueJS())
  }
  widget$setValueJS <- function(., ...) {
    out <- String() +
      paste(sprintf("var ans = [%s];", paste(tolower(as.character(.$..data)), collapse=",")),
            sprintf("for(var i=0; i < %s; i++) {", .$length()),
            sprintf("  %s.getComponent(i).setValue(ans[i]);", .$asCharacter()),
            sprintf("};"),
            collapse="")
      ## 'var ans = [' + paste(tolower(as.character(.$..data)),collapse=",") +
      ##   '];' +
      ##     'for( var i = 0; i < ' + .$length() + ';i++) {' +
      ##       .$asCharacter() + '.getComponent(i).setValue(ans[i]);' +
      ##         '};'
    return(out)
  }

  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we need to reverse logic from AWidgtet$getValue
    out <- .$..data

    index <- getWithDefault(index, FALSE)
    if(index)
      return(which(out))                # indices -- not logical
    else
      return(.$..values[out])
  }

  widget$xtype <- "checkbox"
  widget$transportSignal <- "check"
  widget$checked <- function(.,i) .$..data[i]

  widget$ExtCfgOptions <- function(.) {
    out <- list(border = FALSE,
                bodyStyle = list(padding = "5px"),
                items = .$makeItems()
                )
    if(.$..horizontal)
      out[['layout']] <- "column"
    
    return(out)
  }

  ## value is array of logicals to transport  back
  widget$transportValue <- function(.,...) {
    out <- String() +
      paste('var value = new Array();',
            sprintf('for(var i = 0; i < %s; i++) {', length(.)),
            sprintf('value[i] = %s.getComponent(i).getValue();', .$asCharacter()),
            '};',
            sep="")
    return(out)
  }



  if(length(checked) != length(items))
    checked <- rep(checked, length=length(items))
  widget$setValues(value = items)       # need values before value!
  widget$setValue(value= checked) ## store logical vector -- might be string

  widget$addHandlerChanged <- function(., handler, action=NULL)
    .$addHandler(signal="check", handler, action)

  container$add(widget, ...)
  
  if(!is.null(handler))
    addHandlerChanged(widget, handler=handler, action=action)

  invisible(widget)

}



##################################################
## gcheckboxgroup with table
##'
##' @param items A vector (or data frame with items to select from)
##' @param checked Logical indicating if values are checked. Recyled. Only FALSE works now!
##' @param handler handler to call when check is done (or undone)
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of parent container
##' @TODO checked isn't working; [<- isn't working, is [?
gcheckboxgrouptable <- function(items,
                                 checked = FALSE,
                                 handler = NULL, action = NULL,
                                 container = NULL, ...) {

  widget <- EXTComponentWithStore$new(toplevel=container$toplevel)
  
  class(widget) <- c("gCheckboxGroupTable",class(widget))
  
  ## set up store
  store <- EXTStore$new(toplevel=container$toplevel)
  store$ID <- container$newID()       # set ID
  
  ## load in items
  if(!is.data.frame(items)) {
    items <- data.frame(items, stringsAsFactors=FALSE)
  }
  
  items <- cbind("__index"=seq_len(nrow(items)), items) # a data frame
  store$data <- items
  widget$..store <- store


  ## assign vlaue
  ## we untaint by coercion to integer indices
  widget$assignValue <- function(., value) {
    value <- value$value    
    .$..data <- sort(as.integer(value))
  }
  

  ## values refer to indices
  widget$setValue <- function(., index=NULL,..., value) {
    ## if index --
    index <- getWithDefault(index, is.numeric(value))
    if(index) {
      .$..data <- as.integer(value)
    } else if(is.logical(value)) {
      .$..data <- which(value)
    } else {
      ## match on first column
      values <- .$..store$data[,1, drop=TRUE]
      .$..data <- which(as.character(value) %in% as.character(values))
    }
    
    ## now process if shown
    if(exists("..shown",envir=., inherits=FALSE)) 
      .$addJSQueue(.$setValueJS(index=index, ...))
  }


  widget$setValueJS <- function(., ...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

    ind <- .$..data                     # indices
    if(length(ind))
      out <- sprintf("%s.getSelectionModel().selectRows(%s);", .$asCharacter(), toJSON(ind - 1))
    else
      out <- sprintf("%s.getSelectionModel().selectRows([]);", .$asCharacter())

    return(out)
   }

    
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we store value as an index
    ind <- as.numeric(.$..data)

    index <- getWithDefault(index, FALSE)

    if(length(ind) == 0) {
      if(index)
        return(numeric(0))
      else
        return(NA)
    }

    
    ## no index -- return values
    if(index) {
      return(ind)
    } else {
      ## depends on drop
      values <- .$..store$data
      values <- values[,-1, drop=FALSE]             # drop __index
      if(is.null(drop) || drop) {
        return(values[ind,1,drop=TRUE])
      } else {
        return(values[ind,])
      }
    }      
  }

  ## should have same dimension as items
  ## i,j ignored here
  widget$setValues <- function(.,i,j,...,value) {
    items <- value
    items <- cbind("__index"=seq_len(nrow(items)), items)
    .$..store$data <- items

    if(exists("..shown",envir=., inherits=FALSE))
      .$addJSQueue(.$setValuesJS(...))
  }

  ## need code to update
  widget$setValuesJS <- function(., ...) {
    ### XXX what to do to set values?
  }
  
  widget$transportSignal <- 'cellclick'

  widget$ExtConstructor <- "Ext.grid.GridPanel"
  ## convenience like asCharacter
  widget$selectionModel <- function(.) String() + sprintf("o%sSelectionModel", .$ID)

  widget$header <- function(.) {
    out <- String()

    ## write out selection model instance
    out <- out + sprintf("\n\n%s = new Ext.grid.CheckboxSelectionModel({});\n", .$selectionModel())

    return(out)
  }

  widget$footer <- function(.) {
    out <- String()

    ## not working?
    ## Calls handler but does not check states
    ## Set initial selection
    if(length(ind <- .$getValue(index=TRUE))) {
      out <- out +
        sprintf("o%sSelectionModel.suspendEvents();", .$ID) +
          sprintf("o%sSelectionModel.selectRows(%s);", .$ID,
                  toJSON(ind - 1)) +
                    sprintf("o%sSelectionModel.resumeEvents();\n", .$ID) 
    }  
    return(out)
  }

  ## similar to gtable, only we specify selection model
  widget$ExtCfgOptions <- function(.) {
    out <- list(store = String(.$..store$asCharacter()),
                columns = String(.$makeColumnModel()),
                stripeRows = TRUE,
                enableRowBody = TRUE, 
                frame = FALSE
                ,autoExpandColumn=tail(names(.$..store$data), n=1)
                ) ## also autoExpandColumn, XXX

    ## The selection model is the checkbox selection model
    out[["sm"]] <- .$selectionModel()

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

  ## modified slightly from gdf to put in selection model for first column
  widget$makeColumnModel <- function(.) {
    
    
    df <- .$..store$data
    colNames <- names(df)[-1]           # no __index
    colNames <- shQuoteEsc(colNames)

    
    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
                 if(!.$has_slot("..columnWidths")) "" else 
                        sprintf(", width: %s",rep(.$..columnWidths, length.out=ncol(df))),
                 ', dataIndex:',colNames,
                 '}',
                 sep="")
    out <- paste('[\n',
                 .$selectionModel(), ",", # add this in from gtable
                 paste(tmp,collapse=",\n"),
                 ']',
                 collapse="")

    return(out)
  }




  ##' override need to put  on selection model
  widget$writeHandlerJS <- function(.,signal="",handler=NULL) {
    if(is.null(signal))                   # errors?
      return()
    out <- String()

    out <- out +
      paste(
            sprintf("%s.on('selectionchange',",.$selectionModel()),
            "function(selModel) {",
            '  var value = new Array();',
            '  if(selModel.hasSelection()) {',
            '    var sels =  selModel.getSelections();',
            '    for(var i = 0, len=sels.length; i < len; i++) {',
            '      var record = sels[i];',
            '      var data = record.get("__index");',
            '      value[i] = data;',
            '    };',
            '  };',
            sprintf("  _transportToR('%s', Ext.util.JSON.encode({value:value}) );",.$ID),
            "},",
            "this, {delay:1,buffer:1, single:false});",
            sep="")
    
    if(!is.null(handler)) {
      out <- out +
        sprintf("%s.on('selectionchange',", .$selectionModel()) +
            .$writeHandlerFunction(signal=signal, handler=handler) +
              ',this, {delay:100,buffer:100, single:false});' + '\n'
    }
    
    return(out)
  }
  

  ## changed = clicked
  widget$addHandlerClicked <- widget$addHandlerChanged <-function(.,handler, action=NULL, ...) {
    .$addHandler(signal="selectionchange",
                 handler = handler,
                 action = action,
                 handlerArguments = "selModel"
                 )
  }


  ## set up widget
  n <- nrow(items); checked <- rep(checked, length.out=n)
  widget$setValue(value = which(checked), index=FALSE)
  
  
  if(!is.null(handler))
    widget$addHandlerChanged(handler, action=action)

  ##
  container$add(widget,...)

  
  invisible(widget)
  
}
