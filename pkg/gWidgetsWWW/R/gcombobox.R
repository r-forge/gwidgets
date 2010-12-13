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


## gcombobox aka gdroplist
## XXX -- needs two or more values

##' combobox implementation
##'
##' The \code{svalue<-} method is used to specify value by name or by
##' index. The \code{[<-} method can be used to update the data to
##' select from.
##' @param items a vector of items to choose from. Or a data frame with 1 column (items), two columns (items, icons), or three columns (items, icons, tooltip)
##' @param selected initially selected item, by index. Use \code{0L} for none.
##' @param editable logical. Does combobox allow editing
##' @param coerce.with Function. If given, called on value before returning
##' @param handler handler
##' @param action action
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @note See the  \code{..tpl} to modify template for what is
##' displayed. Override \code{..hideTrigger} and \code{..typeAhead} to
##' change behaviours.
##' @export
gcombobox <- function(items, selected=1, editable=FALSE, coerce.with=NULL,
           handler = NULL, action = NULL, container=NULL,...) {

    widget <- EXTComponentWithStore$new(toplevel=container$toplevel,
                               ..editable = editable,
                               ..selected = selected)
    class(widget) <- c("gComboBox",class(widget))
    
    store <- EXTStore$new(toplevel=container$toplevel)
    store$ID <- container$newID()       # set ID

    ## we have possible a multicolumn items
    ## we want
    ## a) vector or 1-col -- just text
    ## b) 2 cols: first text, second url of icon
    ## c) 3 cols: first text, second url, third quick tip
    ## d) 4 cols: user can use template


 
    
    if(!is.data.frame(items) ||  ncol(items) == 1)
      widget$..type <- 1
    else
      widget$..type <- ncol(items)

    if(!is.data.frame(items)) {
      if(is.numeric(items))
        widget$coerce.with = "as.numeric"
      items <- data.frame(values=items, stringsAsFactors=FALSE)
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

    ## we redefine the setvalue bit. We use the raw value here so that
    ## editable and not are the same. In get value we coerce to index,
    ## if possible, to make sure value is one we want
    widget$assignValue <- function(., value) {
      .$..data <- value[[1]]
    }

    ##' we override get value to make sure we check that value is in
    widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
      ## we store value as an index
      index <- getWithDefault(index, FALSE)
      
      out <- .$..data
      values <- .$getValues()
      ## hack to make chosenCol work with combobox
      chosenCol <- 1
      values <- values[, chosenCol, drop=TRUE]

      ## if editable then we just return
      if(.$..editable) {
        if(index) {
          ind <- values %in% out
          if(any(ind))
            return(ind[1])
          else
            return(0L)
        } else {
          return(out)
        }
      }

      ## otherwise, we get the index first (to untaint) then go from there
      ind <- which(as.character(values) %in% as.character(out))

      if(length(ind) == 0)              # no match
        if(index)
          return(0)
        else
          return("")                    # not editable, so we clobber

      ind <- ind[1]
      ## no index -- return values
      if(index) {
        return(ind)
      } else {
        values <- .$..store$data
        ## depends on drop
        if(names(values)[1] == "__index")
          values <- values[,-1, drop=FALSE]             # drop __index
        
        if(is.null(drop) || drop) {
          return(values[ind, chosenCol, drop=TRUE])
        } else {
          return(values[ind,])
        }
      }      
    }


    widget$setValue <- function(., index=NULL, ..., value) {
      
      ## can set by text or by index.
      index <- getWithDefault(index, FALSE)
      if(index) {
        values <- .$getValues()
        if(value >= 1)
          value <- values[value,1]        # get from data frame
        else
          value = ""                    # empty if selected = 0
      }
      .$..data <- value
      
      ## now process if shown
      if(.$has_local_slot("..shown"))
        .$addJSQueue(.$setValueJS(index=index, ...))
    }
    
    widget$setValueJS <- function(., ...) {
      if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
      ind <- .$getValue(index=TRUE)
      
      if(ind <= 0)
        out <- sprintf("%s.clearValue()", .$asCharacter())
      else
        out <- sprintf("%s.setValue('%s');", .$asCharacter(), .$getValue(index=FALSE))
  return(out)
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
        ##cat(.$setValuesJS(...), file=stdout())
        .$addJSQueue(.$setValuesJS(...))
    }
    widget$ExtConstructor <- "Ext.form.ComboBox"
    widget$ExtCfgOptions <- function(.) {
      out <- list(renderTo = NULL,      # override
                  id = as.character(String(.$ID) + "item"),
                  xtype = "combo",
                  store = String(.$..store$asCharacter()),
                  displayField = .$..store$displayField(),
                  valueField = .$..store$displayField(),
                  editable = .$..editable,
                  mode = "local",
                  triggerAction = "all",
                  hideTrigger  = .$..hideTrigger,
                  typeAhead = .$..typeAhead,
                  emptyText = .$..emptyText,
                  selectOnFocus = TRUE,
                  tpl =  .$..tpl
                  )
      if(!is.na(svalue(.)))
        out[['value']] <- svalue(.)     # string, not index
      
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
                    sprintf('renderTo: %s,', .$toplevel$..renderTo) +
                      'items: [' + '\n' + 
                        .$mapRtoObjectLiteral() +
                          ']' + '\n' +
                            '});' + '\n'

      out <- out + sprintf("o%spanel.addClass('x-hidden');\n", .$ID)

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
        
    
    ## initialize after methods are defined.
    ## This is why we need to make a Trait
    if(selected >= 1) {
      widget$setValue(value=items[selected,1,drop=TRUE]) # items is a data frame!
    } else {
      widget$setValue(value="")         # selected == 0 --> no entry
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

##' Old name for widget
##'
##' Deprecated
gdroplist <- gcombobox
