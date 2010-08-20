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
                container = NULL, ...) {

  
  widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                                      ..name = name,
                                      ..do.subset = do.subset
                                      )
  class(widget) <- c("gDf",class(widget))

  ## process width, height if given
  theArgs <- list(...)
  if(!is.null(theArgs$width)) widget$..width <- theArgs$width
  if(!is.null(theArgs$height)) widget$..height <- theArgs$height

  
  ## set up store
  store <- EXTStore$new()
  store$ID <- container$newID()       # set ID

  ## load in items
  if(!is.data.frame(items)) items <- as.data.frame(items)
  
  store$data <- items
  widget$..store <- store

  ## set up widget

  widget$getValues <- function(., ...) {
    ## transport puts new values into
    ## "gWidgetIDX.R.C" where R = row, C = column
    ## all values stored as characters!

    ## first we pt the new values into the data frame
    items <- .$..store$data

    newVals <- ls(pat = String("^") + .$ID + '\\.', envir=.$toplevel)
    if(length(newVals) > 0) {
      for(i in newVals) {
        tmp <- unlist(strsplit(i, "\\."))
        row <- as.numeric(tmp[2])
        col <- as.numeric(tmp[3])
        value <- get(i, envir=.$toplevel)

        colType <- class(items[,col, drop=TRUE])[1]

        value <- switch(colType,
                        "numeric" = as.numeric(value),
                        "integer" = as.numeric(value),
                        "logical" = as.logical(value),
                        as.character(value))
        items[row,col] <- value

        rm(list = i, envir=.$toplevel)                           # remove
      }
    }

    ## update data store
    .$..store$data <- items

    return(items)

  }

  
  widget$transportSignal <- c("afteredit")
  widget$transportValue <- function(.,...) {
    ## XXX use id.row.col <- value to update cell
  }
  widget$writeTransport <- function(.,ext="",signal=NULL) {
  ## transport to R
    ## We have rowIndex, colIndex and value to work with here
    ## write to id.row.col

    out <- String() +
      'var rowIndex = e.row + 1; var colIndex = e.column + 1;' +
        'var id = "' + .$ID + '" + "." + rowIndex.toString() + "." + colIndex.toString();' +
          '_transportToR(id, Ext.util.JSON.encode({value:e.value}));'
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

    mapEditor <- function(x) {
      type <- class(x)[1]
      switch(type,
             "integer" = ",editor: new Ext.form.NumberField({allowBlank: true,allowDecimals: false,nanText: 'NA'})",
             "numeric" = ",editor: new Ext.form.NumberField({allowBlank: true,allowDecimals: true,nanText: 'NA'})",
             "logical" = String(",editor:") + "new Ext.form.ComboBox({typeAhead: true,editable: false,triggerAction: 'all',store: ['true','false'],lazyRender:true,listClass: 'x-combo-list-small'})",
             "factor" = String(",editor:") + "new Ext.form.ComboBox({typeAhead: true,editable: false,triggerAction: 'all',store: [" + paste(shQuote(levels(x)),collapse=",") + "],lazyRender:true,listClass: 'x-combo-list-small'})",
             "date" = "",               # we create this?
             ",editor: new Ext.form.TextField()") # default is text
    }

    df <- .$..store$data
    editors <- sapply(df, function(i) mapEditor(i))
    colNames <- names(df)
    colNames <- shQuoteEsc(colNames)

    
    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
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
    widget$addHandler(signal = "afteredit", handler, action=action, ...)
  
  invisible(widget)
  
}
