## gcombobox aka gdroplist
## XXX -- needs two or more values

gcombobox <- gdroplist <- 
  function(items, selected=1, editable=FALSE, coerce.with=NULL,
           handler = NULL, action = NULL, container=NULL,...) {

    widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                               ..editable = editable,
                               ..selected = selected)
    class(widget) <- c("gComboBox",class(widget))
    
    store <- EXTStore$new()
    store$ID <- container$newID()       # set ID

    ## we have possible a multicolumn items
    ## we want
    ## a) vector or 1-col -- just text
    ## b) 2 cols: first text, second url of icon
    ## c) 3 cols: first text, second url, third quick tip
    ## d) 4 cols: user can use template


    ## figure out which type
    if(!is.data.frame(items) ||  ncol(items) == 1)
      widget$..type <- 1
    else
      widget$..type <- ncol(items)

    if(!is.data.frame(items)) {
      if(is.numeric(items))
        widget$coerce.with = "as.numeric"
      items <- data.frame(value=items, stringsAsFactors=FALSE)
    }
    
    ## double up first column
    items <- items[, c(1, 1:ncol(items))]
    
    ## get names right
    ## if not type 1,2 or 3 or we override in ..tpl this is ignored
    nms <- c("value","text","iconurl","qtip")
    if(widget$..type %in% 1) {
      if(ncol(items) == 1)
        items[,2] <- items[,1]
      names(items) <- nms[1:2]
    } else if(widget$..type == 2) {
      names(items) <- nms[1:3]
    } else if(widget$..type >= 3) {
      names(items)[1:4] <- nms
    }

    ## fix up icons
    if(widget$..type >= 2) {
      if(!isURL(items[1,3,drop=TRUE])) {
        ## assume a stock icon
        items[,3] <- getStockIcons(items[,3,drop=TRUE])
      }
    }
    
    ## store has a data frame for its "data" property
    store$setData(items)
    store$setChosenCol(store$fieldNames()[1])

    widget$..store <- store

    if(selected >= 1) {
      widget$setValue(value=items[selected,1,drop=TRUE]) # items is a data frame!
    } else {
      widget$setValue(value="")         # selected == 0 --> no entry
    }
##    widget$setValues(value=items) ## done in store not here

    ## properties 
    widget$..width <- 200
    widget$..emptyText <- ""
    widget$..hideTrigger <- FALSE
    widget$..typeAhead <- FALSE
    ## return a string
    widget$..tpl <- function(.) {
      ## return string
      if(.$..type == 1) {
        out <- String() + '<div class="x-combo-list-item">{text}</div>'
      } else if(.$..type == 2) {
        out <- String() + '<div class="x-combo-list-item"><img src="{iconurl}">{text}</div>'
      } else if(.$..type >= 3) {
        out <- String() + '<div ext:qtip="{qtip}" class="x-combo-list-item"><img src="{iconurl}">{text}</div>'
      }

      out <- String('\'<tpl for=".">') + out + '</tpl>\''
      return(out)
    }
    
    ## CSS
    ## Scripts

    ## methods

    widget$getValueJSMethod <- "getRawValue"
    widget$setValueJSMethod <- "setValue"
    widget$..setValue <- function(., index=NULL, ..., value) {
      ## can set by text or by index.
      if(!is.null(index) && index) {
        values <- .$getValues()
        if(value >= 1)
          value <- values[value,1]        # get from data frame
        else
          value = ""                    # empty if selected = 0
      }
      .$..data <- as.character(value)
      if(.$ID != "")
        assign(.$ID,value, envir=.$toplevel)
    }
    

    widget$setValues <- function(.,i,j,...,value) {
      ## intercept value if not data frame or if only 1 d
      if(!is.data.frame(value)) {
        value <- data.frame(values=value, labels=value,stringAsFactors=FALSE)
      } else if(ncol(value) == 1) {
        value <- data.frame(values = value[,1,drop=TRUE], labels  = value[,1,drop=TRUE])
      } else {
        ## double up
        value <- value[,c(1,1:ncol(value))]
      }
        

      
      ## XXX need to include i,j stuff
      .$..store$data <- value
      if(exists("..shown",envir=., inherits=FALSE))
        cat(.$setValuesJS(...), file=stdout())
    }
    widget$ExtConstructor <- "Ext.form.ComboBox"
    widget$ExtCfgOptions <- function(.) {
      out <- list(renderTo = NULL,      # override
                  id = as.character(String(.$ID) + "item"),
                  xtype = "combo",
                  store = String(.$..store$asCharacter()),
                  displayField = .$..store$displayField(),
                  valueField = .$..store$displayField(),
                  value = svalue(.),
                  editable = .$..editable,
                  mode = "local",
                  triggerAction = "all",
                  hideTrigger  = .$..hideTrigger,
                  typeAhead = .$..typeAhead,
                  emptyText = .$..emptyText,
                  selectOnFocus = TRUE,
                  tpl =  .$..tpl
                  )

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

    ## we need to override methods of EXTComponent$Show
    widget$writeConstructor <- function(.) {
      out <- String() +
        'o' + .$ID + 'panel = new Ext.Panel({' + # no var -- global
          'id:' + shQuote(.$ID) + ',' +
            'xtype:"panel",' +
              'layout:"fit",' +
                'width:' + .$..width + ',' +
                  'height: "auto",' +
                    'renderTo: Ext.getBody(),' +
                      'items: [' + '\n' + 
                        .$mapRtoObjectLiteral() +
                          ']' + '\n' +
                            '});' + '\n'
      out <- out +
        'o' + .$ID + ' = ' + # no var -- global
                    'o' + .$ID + 'panel.getComponent(0);' + '\n'

      return(out)
    }

    widget$transportSignal <- c("blur" ,"select", "change")
    widget$transportValue <- function(., ..., signal=NULL) {
      out <- String()
      if(signal == "change") {
        out <- out + 'var value = newValue;' + '\n'
      } else {
        out <- out + 'var value = o' + .$ID + '.getRawValue();' + '\n'
      }
      return(out)
    }
        
    

    
    ## methods

    container$add(widget,...)

    widget$addHandlerChanged <- function(.,handler=NULL, action=NULL, ...) {
      .$addHandler("select",handler=handler,action=action)
      .$addHandler("change",handler=handler,action=action)
    }
      
    if(!is.null(handler))
      widget$addHandlerChanged(handler, action=action)
    
    
    invisible(widget)
  }
