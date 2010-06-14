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
## integrate -- filter fun: maybe never

gtable <- function(items, multiple = FALSE, chosencol = 1,
                   icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL,
                   filter.FUN = NULL, handler = NULL, action = NULL,
                   container = NULL, ...) {

  widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                                      ..multiple = multiple,
                                      ..icon.FUN = icon.FUN
                                      )

  class(widget) <- c("gTable",class(widget))

  theArgs <- list(...)
  
  ## set up store
  store <- EXTStore$new()
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
  items <- cbind(..index=seq_len(nrow(items)), items)
  store$data <- items
  widget$..store <- store

  ## set up widget
  widget$setValue(value = 1)            # first column is selected on startup
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we store value as an index
    out <- .$..data
    values <- .$..store$data

    if(exists("..shown",envir=.,inherits=FALSE)) {
      ## get from widget ID
      out <- try(get(.$ID,envir=.$toplevel),silent=TRUE) ## XXX work in index here?
      if(!inherits(out,"try-error")) {
        .$..data <- out                 # update data
      } else {
        out <- .$..data
      }
    }
    ## no index -- return values
    if(!is.null(index) && index) {
      return(as.numeric(out))
    } else {
      ## depends on drop
      values <- values[,-1]             # drop ..index
      if(is.null(drop) || drop) {
        return(values[as.numeric(out),.$..store$chosenCol,drop=TRUE])
      } else {
        return(values[as.numeric(out),])
      }
    }      
  }
  
  widget$setValueJS <- function(., ...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

    ind <- .$getValue(index=TRUE, drop=TRUE)
    out <- String() +
       'o' + .$ID + '.getSelectionModel().selectRows(' + toJSON(ind - 1) + ');' # offset by 1

    return(out)
   }

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
    items <- cbind("..index"=seq_len(nrow(items)), items)
    .$..store$data <- items

    if(exists("..shown",envir=., inherits=FALSE))
      cat(.$setValuesJS(...), file=stdout())
  }

  ##' visibility
  widget$setVisible <- function(., value) {
    ## XXX nothing to do here, can't find the visible (setHidden? method we need)

  }

  ## we don't have a good means for visible<-. Instead we have this
  ## filter proto method 
  widget$filter <- function(., colname, regex) {
    if(!exists("..shown",envir=., inherits=FALSE)) {
      ## "Can only filter once object is shown"
      out <- ""
    }

    if(missing(colname) || !colname %in% names(.$..store$data))  {
       ## Need colname to match one of the names of the data set
      out <- ""
    }

    if(missing(regex) || regex=="") {
      out <- sprintf("o%s.getStore().clearFilter();", .$ID)
    } else {
      out <- sprintf("o%s.getStore().filter('%s','%s');", .$ID, colname, regex)
    }
    cat(out, file=stdout())
  }
  

  
  widget$transportSignal <- c("cellclick")
  widget$transportValue <- function(.,...) {
    ## we packed in ..index so we can get the index even if we've sorted
    if(.$..multiple) {
       ## work a bit to get the value
       out <- String() +
         'var store = w.getStore();' +
           'var selModel = w.getSelectionModel();' +
             'var values = selModel.getSelections();' +
               'var value = new Array();' +
                 'for(var i = 0, len=values.length; i < len; i++) {' +
                   'var record = values[i];' +
                     'var data = record.get("..index");' +
                         'value[i] = data' +
                           '};'
     } else {
       out <- String() +
         'var record = w.getStore().getAt(rowIndex);' +
           'var value = record.get("..index");' 
     }
    return(out)
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
    
    ## paging -- doesn't work -- doesn't get no. of fields from store
##     out[['bbar']] = String() +
##       paste("new Ext.PagingToolbar({",
##             "pageSize: 10,",
##             (String("store:") + .$..store$asCharacter() + ','),
##             "displayInfo: true,",
##             "displayMsg: 'Displaying topics {0} - {1} of {2}',",
##             "emptyMsg: 'No topics to display',",
##             "items:[",
##             "'-', {",
##             "pressed: true,",
##             "enableToggle:true,",
##             "text: 'Show Preview',",
##             "cls: 'x-btn-text-icon details',",
##             "toggleHandler: function(btn, pressed){",
##             (String("var view =") + .$asCharacter() + ".getView();"),
##             "view.showPreview = pressed;",
##             "view.refresh();",
##             "}",
##           "}]",
##           "})",
##             collapse="\n")

    
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

  ## make some renderes
  widget$scripts <- function(.) {
    out <- String()
    ## text is red or black depending
    out <- out + "\n" +
      'gtableNumeric = function(val) { ' +
        'return \'<span style="color:red">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableInteger = function(val) { ' +
        'return \'<span style="color:red">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableLogical = function(val) { ' +
        'return \'<span style="color:black">\' + val + \'</span>\';' +
          '};' + '\n'
    out <- out +
      'gtableIcon = function(val) { ' +
        'return \'<img src="\' + val + \'"/>\';' +
          '};' + '\n'

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

    mapRenderer <- function(type) {
      switch(type,
             "character"="",
             "String" = "",
             "integer" = ",renderer:gtableInteger",
             "numeric" = ",renderer:gtableNumeric",
             "logical" = ",renderer:gtableLogical",
             "factor" = "",
             "icon" = ",width: 16,renderer:gtableIcon",              # for icons(we create this)
             "date" = "",               # we create this?
             "")
    }

    df <- .$..store$data
    renderers <- sapply(df[,-1, drop=FALSE], function(i) mapRenderer(class(i)[1]))
    colNames <- names(df)[-1]           # XXX
##    colNames <- names(df)
    colNames <- shQuoteEsc(colNames)

    ## widths
    fontWidth <- 10
    colWidths <- sapply(df[,-1, drop=FALSE], function(i) max(nchar(as.character(i))) + 1)
    colWidths <- pmax(colWidths, nchar(names(df[,-1, drop=FALSE])) + 1)
    totalWidth <- ifelse(exists("..width", envir=., inherits=FALSE), .$..width, "auto")
    if(totalWidth == "auto" || fontWidth * sum(colWidths) > totalWidth)
      colWidths <- colWidths * fontWidth       # fontWidth pixels per character
    else
#      colWidths <- floor(fontWidth * colWidths * totalWidth/sum(colWidths))
      colWidths <- colWidths * fontWidth       # fontWidth pixels per character
    
    ## didn't work for header:
    trimDD <- function(x) {
      ind <- grep("^..", x)
      if(length(ind) > 0)
        x[ind] <- "''"
      return(x)
    }

    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
                 ', width:', colWidths,
                 ', dataIndex:',colNames,
                 renderers,
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
    types <- sapply(df[,-1, drop=FALSE], function(i) mapTypes(class(i)[1]))
    colNames <- shQuoteEsc(names(df)[-1])
    tmp <- paste("{name:", colNames, types, "}", sep="")
    out <- paste("[",tmp,"]", collapse="\n")

    return(out)
  }

  widget$footer <- function(.) {
    out <- String() +
      'o' + .$ID + '.getSelectionModel().selectFirstRow();'

    .$Cat(out)
  }
  
  
  ###
  container$add(widget,...)

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
  
  if(!is.null(handler))
    widget$addHandlerChanged(handler, action=action)
  
  
  invisible(widget)
  
}
