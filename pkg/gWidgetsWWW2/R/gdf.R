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

##' @include gtable.R
NA


##' A widget for editing a data frame
##'
##' A widget for editing a data frame. There is no means to change the
##' size of the frame being edited or the type of data in each
##' column. As such, one must plan ahead.
##' @param items data frame to be edited
##' @param name name of data frame appearing in titke
##' @param do.subset Ignored.
##' @param container Parent container
##' @param ... passed to container's \code{add} method
##' @param width width of widget in pixels
##' @param height height of widget in pixels
##' @param ext.args extra arguments to pass to constructor
##' @return an ExtWidget
##' @author john verzani
gdf <- function(items = NULL, name = deparse(substitute(items)),
                do.subset = FALSE,
                container = NULL, ...,
                width=300, height=200,   # gWidgetsWWW defaults
                ext.args = NULL
                ) {

  gd <- GDF$new(container$toplevel)
  gd$init(items, name, container, ..., width=width, height=height, ext.args=ext.args)
  gd
}

##' base class for gdatarframe
##' @name gdf-class
GDF <- setRefClass("GDF",
                   contains="GWidgetGrid",
                   fields=list(
                     "store"= "ANY",
                     "name" = "character"
                     ),
                   methods=list(
                     init=function(items, name, container, ..., width=NULL, height=NULL, ext.args=NULL) {

                       if(!is.data.frame(items))
                         items <- as.data.frame(items, stringsAsFactors=FALSE)

                       store <<- ExtArrayStore$new(container$toplevel)
                       store$init(items)

                       name <<- name
                       nms <<- names(items)
                       
                       constructor <<- "Ext.grid.EditorGridPanel"
                       transport_signal <<- "afteredit"

                       arg_list = list(
                         store = String(store$get_id()),
                         columns = String(store$proxy$make_column_model(do.edit=TRUE)),
                         stripeRows = TRUE,
                         frame = FALSE,
                         title = name,
                         width=width,
                         height=height
                         )

                       add_args(arg_list)
                       if(!is.null(ext.args))
                         add_args(ext.args)
                       
                       container$add_dots(.self, ...)                           
                       write_constructor()
                       write_transport()
                       container$add(.self, ...)

                       ## load data
                       store$load_data()
                     },
                     transport_fun = function() {
                       ## transport back cell
                       "var param = {value:e.value, i:e.row + 1, j:e.column+1}"
                     },
                     process_transport = function(value, i, j) {
                       set_items(value, i, j)
                     },
                     
                     
                     ## can toggle editability of columns
                     set_editable = function(value, column) {
                       "Set a column editable or not. @param value logical, @param column column number, Defaults to all"
                       if(missing(column))
                         column <- 1:dim()[2]
                       sapply(column, function(col) {
                         call_column_method("setEditable", col - 1, value)
                       })
                     }
                    
                       
                     ))


## We need to coerce a value from string to ..

##' Generic to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue <- function(x, value) UseMethod("coerceValue")

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.default <- function(x, value) format(value)

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.character <- function(x, value) as.character(value)

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.integer <- function(x, value) as.integer(value)

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.numeric <- function(x, value) as.numeric(value)

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.logical <- function(x, value)  as.logical(toupper(value))

##' method to coerce value before assigning into data frame
##' @param x what value will go into
##' @param value to set into x
coerceValue.factor <- function(x, value) ifelse(value %in% levels(x), value, NA)
