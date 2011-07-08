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


## use EditorGridView to edit a data frame

## working

## svalue
## transport
## click and double click handler
## icon fun: use class(icon) to mark
## multiple is working
## [<- : needs to have data frame with same column types, no. of columns

## get working:
## names<-, names for headers
## integrate -- filter fun: maybe never

     
gdf <- function(items = NULL, name = deparse(substitute(items)),
                do.subset = FALSE,
                container = NULL, ...,
                width=200, height=200   # gWidgetsWWW defaults
                ) {

  
  widget <- EXTComponentDfStore$new(toplevel=container$toplevel,
                                      ..name = name,
                                      ..do.subset = do.subset,
                                    ..width = width,
                                    ..height= height
                                      )
  class(widget) <- c("gDf",class(widget))



  
  ## set up store
  store <- EXTStore$new(toplevel=container$toplevel)
  store$ID <- container$newID()       # set ID

  ## load in items
  if(!is.data.frame(items)) items <- as.data.frame(items)
  
  store$data <- items
  widget$..store <- store
  widget$..data <- numeric(0)           # we store indices
  ## set up widget


  widget$assignValue <- function(., value) {
    ## assign value. Here values i list value, row, column
    coerceValue <- function(x, value) UseMethod("coerceValue")
    coerceValue.default <- function(x, value) format(value)
    coerceValue.character <- function(x, value) as.character(value)
    coerceValue.integer <- function(x, value) as.integer(value)
    coerceValue.numeric <- function(x, value) as.numeric(value)
    coerceValue.logical <- function(x, value)  as.logical(toupper(value))
    coerceValue.factor <- function(x, value) ifelse(value %in% levels(x), value, NA)
  

    df <- .$..store$data
    i <- as.numeric(value[[2]])
    j <- as.numeric(value[[3]])
    val <- value[[1]]
    df[i,j] <- coerceValue(df[[j]], val)
    .$..store$data <- df
  }

  ## return data frame
  widget$getValues <- function(., ...) {
    items <- .$..store$data
    return(items)
  }

  ## transport
  widget$transportSignal <- c("afteredit")
  widget$transportValue <- function(.,...) {
    ## XXX use id.row.col <- value to update cell
  }
  widget$writeTransport <- function(.,ext="",signal=NULL) {
  ## transport to R
    ## We have rowIndex, colIndex and value to work with here
    ## write to id.row.col

    out <- String() +
      sprintf("_transportToR('%s', Ext.util.JSON.encode({value:e.value,row:e.row + 1, column:e.column+1}));", .$ID)
    return(out)
  }

  widget$ExtConstructor <- "Ext.grid.EditorGridPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(store = String(.$..store$asCharacter()),
                columns = String(.$makeColumnModel()),
                stripeRows = TRUE,
                frame = FALSE
                ) ## also autoExpandColumn, XXX
    if(.$..name != "")
      out[["title"]] <- .$..name
    
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

  ## The map editor
  widget$mapEditor <- function(., x) {
    type <- class(x)[1]
    switch(type,
           "integer" = ",editor: new Ext.form.NumberField({allowBlank: true,allowDecimals: false,nanText: 'NA'})",
           "numeric" = ",editor: new Ext.form.NumberField({allowBlank: true,allowDecimals: true, nanText: 'NA'})",
           "logical" = String(",editor:") + "new Ext.form.ComboBox({typeAhead: true,editable: false,triggerAction: 'all',store: ['true','false'],lazyRender:true,listClass: 'x-combo-list-small'})",
           "factor" = String(",editor:") + "new Ext.form.ComboBox({typeAhead: true,editable: false,triggerAction: 'all',store: [" + paste(shQuote(levels(x)),collapse=",") + "],lazyRender:true,listClass: 'x-combo-list-small'})",
           "date" = "",               # we create this?
           ",editor: new Ext.form.TextField()") # default is text
  }

  widget$makeColumnModel <- function(.) {
    ## return array for columns
    ## id, header, sortable, renderer, dataIndex, tooltip
##     columns: [
##               {id:'company',header: "Company", sortable: true, dataIndex: 'company'},
##               {header: "Price",  sortable: true, renderer: 'usMoney', dataIndex: 'price'},
##               {header: "Change", sortable: true, renderer: change, dataIndex: 'change'},
##               {header: "% Change", sortable: true, renderer: pctChange, dataIndex: 'pctChange'},
##               {header: "Last Updated", sortable: true, renderer: Ext.util.Format.dateRenderer('m/d/Y'), dataIndex: 'lastChange'}
##               ],


    df <- .$..store$data
    editors <- sapply(df, function(i) .$mapEditor(i))
    colNames <- names(df)
    colNames <- shQuoteEsc(colNames)

    
    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
                 if(!.$has_slot("..columnWidths")) "" else 
                        sprintf(", width: %s",rep(.$..columnWidths, length.out=ncol(df))),
                 ', dataIndex:',colNames,
                 editors,
                 '}',
                 sep="")
    out <- paste('[\n', paste(tmp,collapse=",\n"), ']', collapse="")

    return(out)
  }
  widget$makeFields <- function(.) {
    ## return something like this with name, type
    ##     fields: [
    ##            {name: 'company'},
    ##            {name: 'price', type: 'float'},
    ##            {name: 'change', type: 'float'},
    ##            {name: 'pctChange', type: 'float'},
    ##            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'}
    ##         ]
    ## types in DataField.js
    mapTypes <- function(type) {
      switch(type,
             "character"="",
             "String" = ",type: 'string'",
             "integer" = ",type: 'int'",
             "numeric" = ",type: 'float'",
             "logical" = ",type: 'boolean'",
             "factor"  = "",
             "date" = ",type:date",
             "")
    }
    df <- .$..store$data
    types <- sapply(df, function(i) mapTypes(class(i)[1]))
    colNames <- shQuoteEsc(names(df))
    tmp <- paste("{name:", colNames, types, "}", sep="")
    out <- paste("[",tmp,"]", collapse="\n")

    return(out)
  }

  widget$footer <- function(.) {}
  
  
  ###
  container$add(widget,...)

  ## handler definitions
  widget$addHandlerChanged <- function(., handler, action = NULL, ...)
    .$addHandler(signal = "afteredit", handler, action=action, ...)
  
  invisible(widget)
  
}
