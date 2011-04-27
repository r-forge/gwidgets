##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' @include ext-widget.R
NA

##Class for widgets which use a proxy store

## Stores need methods for a) returning data from a request, b) displaying javascript to make request
## basic idea:
##ogWidget_id1 = proxy(url)
##ogWidget_id(1+1) = store
##ogWidgets_id(1+2) = GridPanel

##' interface between R objects and JSON to supply Ext objects with data
ExtProxy <- setRefClass("ExtProxy",
                        contains="ExtObject",
                        fields=list(
                          value="ANY"
                          ),
                        methods=list(
                          init=function(value, ...) {
                            
                            value <<- value
                            
                            constructor <<- "Ext.data.HttpProxy"
                            arg_list <- get_url_list()
                            arg_list[['method']] <- "GET"
                            add_args(arg_list)
                            
                            write_constructor()
                          },
                          get_url_list = function() {
                            "Get url to call back into this proxy object. The values 'proxy_url' and 'session_id' are JS globals in webpage"
                            list(url=String("proxy_url"),
                                 params=list(
                                   id = get_id(),
                                   session_id=String("session_id")
                                   )
                                 )
                          },
                          get_json_data=function(...) {
                            "The params determine what to pass back. "
                            ## Default is just to json encode value
                            toJSON(value)
                          },
                          get_data=function(...) {
                            "Return raw data, not JSON encoded"
                            value
                          },
                          set_value = function(value, ...) {
                            "Set data and call load function"
                            value <<- value
                            cmd <- "XXX"
##                            add_js_queue(cmd) ## how to notify observers of change?
                          },
                          set_data=function(value, ...) {
                            "Set proxy data"
                            value <<- value
                          },
                          get_fields=function() {
                           "Return json with fields mapped to names"
                           ""
                          }
                          ))

##' Proxy for ghtml
ExtHTMLProxy <- setRefClass("ExtHTMLProxy",
                            contains="ExtProxy",
                            methods=list(
                              get_json_data=function(...) {
                                "Despite the name, return raw data, not JSON encoded"
                                value
                              }
                              ))

## this function is used internally by  make_column_model but for some reason doesn't work
## when defined within that method body
## the rendererers are defined in gw-gtable.js

##' Generic function to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer <- function(x) UseMethod("column_renderer")

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.default <- function(x) list()

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.integer <- function(x) list(renderer=String("gtableInteger"))

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.numeric <- function(x) list(renderer=String("gtableNumeric"))

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.logical <- function(x) list(renderer=String("gtableLogical"))

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.Icon    <- function(x) list(width=20, renderer=String("gtableIcon"))

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.date    <- function(x) list(renderer=String("gtableDate")) ## fix this js


## This function is use by make_column_model when editing of cells is
## desired. They return a configuration list

##' Generic function to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor <- function(x, ...) UseMethod("column_editor")

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.default <- function(x, ...)  {
  list(editor = String("new Ext.form.TextField"))
}

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.integer <- function(x, ...) {
  list(editor =  String(sprintf("new Ext.form.NumberField(%s)",
         toJSObject(list(allowBlank=TRUE, allowDecimals=FALSE, nanText='NA')))))
}

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.numeric <- function(x, ...) {
  list(editor =  String(sprintf("new Ext.form.NumberField(%s)",
         toJSObject(list(allowBlank=TRUE, allowDecimals=TRUE, nanText='NA')))))
}

## should be Checkbox, but can't get to work

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.logical <- function(x, ...) {
  list(editor =  String(sprintf("new Ext.form.ComboBox(%s)",
         toJSObject(list(typeAhead = TRUE,
                         editable=FALSE,
                         triggerAction = "all",
                         store = String("['true', 'false']"),
                         lazyRender=TRUE,
                         listClass="x-combo-list-small"
                         )))))
}
 


##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.factor <- function(x, ...) {
  list(editor =  String(sprintf("new Ext.form.ComboBox(%s)",
         toJSObject(list(typeAhead = TRUE,
                         editable=FALSE,
                         triggerAction = "all",
                         store = String(sprintf("[%s]", paste(sapply(levels(x), ourQuote),collapse=","))),
                         lazyRender=TRUE,
                         listClass="x-combo-list-small"
                         )))))
}
## column_editor.date <- function(x, ...) {}


##' Proxy to return array data (combobox, table, gdf, ...)
ExtArrayProxy <- setRefClass("ExtArrayProxy",
                             contains="ExtProxy",
                             fields=list(
                               col.widths="ANY",
                               update_url_list = "list"
                               ),
                             methods=list(
                                init=function(value, ...) {
                                  
                                  value <<- value
                                  col.widths <<- getFromDots("col.widths", ..., default=NULL)
                                  
                                  constructor <<- "Ext.data.HttpProxy"
                                  arg_list <- get_url_list()
                                  arg_list[['method']] <- "GET"
                                  add_args(arg_list)
                                  
                                  write_constructor()
                                },
                               set_data = function(value, i, j, ...) {
                                 if(missing(i) && missing(j))
                                   value <<- as.data.frame(value, stringsAsFactors=FALSE)
                                 else
                                   value[i,] <<- value
                               },
                               get_json_data=function(...) {
                                 "Return JSON array [[],[],] ..."
                                 df <- cbind("id"=seq_len(nrow(value)), value)
                                 ## do we have paging type request? We do if params$start is not null
                                 params <- list(...)
                                 if(!is.null(params$start)) {
                                   start <- as.numeric(params$start)
                                   limit <- as.numeric(params$limit)
                                   m <- nrow(df)

                                   if(m > 0 && m >= start) {
                                     ind <- seq(start, min(m, start+limit))
                                     df <- df[ind,,drop=FALSE]
                                   }
                                 }
                                 toJSArray(df)
                               },
                               get_fields=function() {
                                 "Return fields mapping from name to type"
                                 if(nrow(value)) {
                                   df <- cbind("id"=seq_len(nrow(value)), value)
                                   makeFields(df)
                                 } else {
                                   ""
                                 }
                               },
                               
                               make_column_model = function(do.editor=FALSE) {
                                 ## return array for columns
                                 ## id, header, sortable, renderer, dataIndex, tooltip
                                 ##     columns: [
                                 ##               {id:'company',header: "Company", sortable: true, dataIndex: 'company'},
                                 ##               {header: "Price",  sortable: true, renderer: 'usMoney', dataIndex: 'price'},
                                 ##               {header: "Change", sortable: true, renderer: change, dataIndex: 'change'},
                                 ##               {header: "% Change", sortable: true, renderer: pctChange, dataIndex: 'pctChange'},
                                 ##               {header: "Last Updated", sortable: true, renderer: Ext.util.Format.dateRenderer('m/d/Y'), dataIndex: 'lastChange'}
                                 ##               ],

                                
                                 
                                 items <- value
                                 n <- ncol(items)
                                 
                                 ## names
                                 colIDs <- names(items)
                                 colNames <- colIDs

                                 ## adjust IDs
                                 colIDs <- gsub("[ \\.\\,] ","_",colIDs)

                                 ## adjust for icons
                                 ind <- sapply(items, isIcon)
                                 if(any(ind)) {
                                   for(i in which(ind)) {
                                     items[,i] <- asIcon(sapply(items[,i], getStockIconByName))
                                     colNames[i] <- NA
                                   }
                                   value <<- items # update                                   
                                 }


                                 sortable <- rep(TRUE, length.out=n)
                                 if(is.null(col.widths))
                                   col.widths <<- rep(NA, length.out=n)

                                 res <- sapply(seq_len(n), function(i) {
                                   l <- list(id=colIDs[i],
                                             header=colNames[i],
                                             sortable=TRUE,
                                             width = col.widths[i],
                                             dataIndex = colIDs[i]
                                             )
                                   if(do.editor)
                                     l <- merge(l, column_editor(items[,i]))
                                   else
                                     l <- merge(l, column_renderer(items[,i]))
                                   toJSObject(l)
                                 })
                                 
                                 out <- sprintf('[%s]', paste(res,collapse=","))
                                 return(out)
                               }
                               
                               ))


##' tree proxy is all we need
ExtTreeProxy <- setRefClass("ExtTreeProxy",
                            contains="ExtArrayProxy",
                            fields=list(
                              offspring="function",
                              offspring.data="ANY",
                              icon.FUN="ANY",
                              chosencol="integer",
                              multiple="logical"
                              ),
                            methods=list(
                              init=function(offspring, offspring.data, icon.FUN, chosencol, multiple) {
                                offspring <<- offspring
                                offspring.data <<- offspring.data
                                icon.FUN <<- icon.FUN
                                chosencol <<- chosencol
                                multiple <<- multiple

                                ## is there a constructor?

                              },
                              set_data = function(value) {
                                "Set data into proxy"
                              },
                              get_json_data = function(node, path) {
                                "return JSON to be read by treeloader"
                                out <- ""
                             
                                if(!is.null(node)) {
                                  ## check if JSON data
                                  ochildren <- children <- offspring(strsplit(node,":")[[1]][-1], offspring_data)
                                  m <- nrow(children)
                                  out <- "[]" # m == 0
                                  if(m > 0) {
                                    ## XX get icon, more than 1 row?
                                    ## append id, then pad
                                    children[,1] <- sprintf("%s:%s", node, gsub(":","_",children[,1]))
                                    if(ncol(children) == 1)
                                      children[,2] <- rep(FALSE, m)
                                    if(ncol(children) == 2)
                                      children[,3] <- rep(NA, m)
                                    if(ncol(children) == 3)
                                      children[,4] <- children[,1]
                                    if(ncol(children) == 4)
                                      children[,5] <- rep(NA, m)
                                    ## these match config options for a Ext.tree.TreeNode
                                    names(children) <- c("id","leaf","iconCls", "text", "qtip")
                                    children$iconCls <- sapply(children$iconCls, getStockIconByName, css=TRUE)
                                    
                                    res <- sapply(seq_len(m), function(i) toJSObject(children[i,]))
                                    out <- sprintf("[%s]", paste(res, collapse=","))
                                  }
                                }
                                return(out)
                              }
                            ))

## Stores are provided when a reader is needed between the component and the proxy.
## LIkely the ArrayStore could be merged in to the proxy, but this is called in combobox, gdf, and gtable
                            
##' Basic store, no default proxy so this isn't useful without being subclassed
ExtStore <- setRefClass("ExtStore",
                        contains="ExtObject",
                        fields=list(
                          proxy="ExtProxy"
                          ),
                        methods=list(
                          init = function(value, ...) {
                            "Initialize proxy, then set up store"
                            proxy <<- ExtProxy$new(toplevel)
                            proxy$init(value, ...)
                            ## ? need to write out store code here...
                          },

                          get_data = function(...) {
                            "Get data in store"
                            proxy$get_data(...)
                          },
                          set_data = function(value, ...) {
                            "Set data for store"
                            proxy$set_data(value, ...)
                          }
                          )
                        )

##' A store for an array (inbetween widget and proxy)
ExtArrayStore <- setRefClass("ExtArrayStore",
                         contains="ExtStore",
                         fields=list(
                           proxy="ExtArrayProxy",
                           page_size = "numeric",
                           paging = "logical"
                           ),
                         methods=list(
                           init = function(df, ...) {
                             if(missing(df))
                               df <- data.frame(name=character(0), stringsAsFactors=FALSE)
                             paging <<- FALSE
                             
                             proxy <<- ExtArrayProxy$new(toplevel)
                             col.widths <- getFromDots("col.widths", ..., default=NULL)
                             proxy$init(df, col.widths)

                             

                             constructor <<- "Ext.data.ArrayStore"
                             arg_list <- list(
                                              proxy = String(.self$proxy$get_id()),
                                              storeId=get_id(),
                                              idIndex=0,
                                              fields=String(.self$proxy$get_fields())
                                           )
                             add_args(arg_list)
                             
                             write_constructor()
                             cmd <- paste(sprintf("%s.on('beforeload', function(store, options) {", get_id()),
                                          sprintf("Ext.apply(options.params, {id:'%s', session_id:session_id});",
                                                  proxy$get_id()),
                                          "});",
                                          sep="")
                             add_js_queue(cmd)
                           },
                           load_data=function() {
                             if(paging)
                               cmd <- sprintf("%s.load({params:{start:0, limit: %s, session_id:session_id, id:'%s'}, add:false, callback:function() {}});",
                                              get_id(), page_size, proxy$get_id())
                             else
                               cmd <- sprintf("%s.load({params:{session_id:session_id, id:'%s'}, add:false, callback:function() {}});",
                                              get_id(), proxy$get_id())
                             add_js_queue(cmd)
                           }
                           )
                         )


## Helper Functions to makeFields.

##' Map a type
##'
##' @param x object
##' @export
mapTypes <- function(x) UseMethod("mapTypes")

##' mapTypes method
##' @param x object
##' @method "mapTypes" default
##' @S3method "mapTypes" default
##' @nord
mapTypes.default <- function(x) list()


##' mapTypes method
##' @param x object
##' @method "mapTypes" String
##' @S3method "mapTypes" String
##' @nord
mapTypes.String <- function(x) list(type="string")

##' mapTypes method
##' @param x object
##' @method "mapTypes" integer
##' @S3method "mapTypes" integer
##' @nord
mapTypes.integer <- function(x) list(type="int")

##' mapTypes method
##' @param x object
##' @method "mapTypes" numeric
##' @S3method "mapTypes" numeric
##' @nord
mapTypes.numeric <- function(x) list(type="numeric")

##' mapTypes method
##' @param x object
##' @method "mapTypes" logical
##' @S3method "mapTypes" logical
##' @nord
mapTypes.logical <- function(x) list(type='boolean')

##' mapTypes method
##' @param x object
##' @method "mapTypes" factor
##' @S3method "mapTypes" factor
##' @nord
mapTypes.factor <- function(x) list()

##' mapTypes method
##' @param x object
##' @method "mapTypes" date
##' @S3method "mapTypes" date
##' @nord
mapTypes.date <- function(x) list()

##' helper function to write field names for Ext constructor
##' @param df data frame
##' @nord
makeFields <- function(df) {
  ##
  ## return something like this with name, type
  ##     fields: [
  ##            {name: 'company'},
  ##            {name: 'price', type: 'float'},
  ##            {name: 'change', type: 'float'},
  ##            {name: 'pctChange', type: 'float'},
  ##            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'}
  ##         ]
  ## types in DataField.js
  ## if(!is.na(ind <- match(".id", names(df))))
  ##   df <- df[, -ind, drop=FALSE]
    
  
  types <- sapply(df, mapTypes)
  colNames <- names(df)
  res <- sapply(seq_len(ncol(df)), function(i) {
    l <- list(name=colNames[i])
    l <- merge.list(l, types[[i]])
    toJSObject(l)
  })
  out <- sprintf("[%s]", paste(res, collapse=","))
  
  return(out)
}


