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
##' @include ext-widget-proxy.R
NA
##' @include icons.R
NA

##' A table widget
##'
##' A widget for displaying a data frame in tabular format. The main
##' property is the set of indices for the currently selected
##' rows. For large data sets, the data can be "paged", that is given
##' to the browser in bite-sized chunks so the lag is lacking.  The
##' change handler is for a single click, also used for selection. Use
##' \code{addHandlerDoublecclick} to specify a callback for the double
##' click event on a cell.
##' 
##' The column names are inherited from the
##' columnnames of the data frame. Names with spaces will render, but
##' are not sortable. Use some punctuation, such as an underscore to
##' get sortability.
##' 
##' A column of class "Icon" (see
##' \code{\link{asIcon}}) will render a css class as an icon. See the
##' example.
##' 
##' The item replacement operator \code{[<-} will work
##' only if all the column types remain constant, as the column
##' renderers are set at time of construction. This also effects the
##' initial data frame. Pass a 0-row data frame with column names and
##' defined column types at construction if no data is known at that
##' point.
##' @param items data frame of items to display
##' @param multiple logical. TRUE for multiple selections
##' @param chosencol The svalue() method returns a single value, by default. This species column of that value.
##' @param icon.FUN NOT IMPLEMENTED. Instead, just add a column with class Icon containing css class of the icons
##' @param filter.column Ignored 
##' @param filter.labels Ignored 
##' @param filter.FUN Ignored. 
##' @param handler single click handlers
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to parent container's \code{add} method
##' @param width width in pixels
##' @param height height in pixels,  should be set, otherwise get only 1 row
##' @param ext.args additional configuration values to pass to constructor
##' @param paging. Logical. If \code{TRUE}, then data will be loaded
##' in chunks -- not all at once. The default page size is 25. This
##' can be adjusted by setting \code{gtable_object$page_size <- 50},
##' say.
##' @param paging Either a logical variable or integer. If \code{TRUE}
##' then paging will be used which allows only chunks of the data to
##' be sent to the browser at a time (default size = 25 rows). If
##' \code{integer} then paging is turned on and this value overrides
##' the default page size.
##' @param col.widths width of each column. Also see \code{size<-}
##' with a list where \code{columnWidths} is specified.
##' @return An ExtWidget instance
##' @note With \code{width} and/or \code{height} set to \code{NULL},
##' the default size will likely be unsatisfying. These values are
##' best set by the programmer. They can be readjusted through the
##' \code{size<-} method. For \pkg{gWidgetsWWW}, the \code{filter}
##' functions are not implemented. Rather the \code{filter} method may
##' be used to filter a named column by a regular expression. See the
##' example.
##' @export
##' @examples
##' w <- gwindow("Filtering and table example")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' g1 <- ggroup(cont=g)
##' glabel("Filter by:", cont=g1)
##' e <- gedit("", cont=g1)
##' tbl <- gtable(data.frame(name=state.name, stringsAsFactors=FALSE), cont=g,  multiple=TRUE)
##' addHandlerKeystroke(e, handler=function(h,...) {
##' val <- svalue(h$obj)
##' if(nchar(val) > 0) {
##' tbl$filter("name", val)
##' }
##' })
##' b <- gbutton("click", cont=g, handler=function(h,...) galert(svalue(tbl, index=FALSE), parent=w))
##' 
##' ## icons
##' m <- mtcars[1:3, 1:4]
##' ## add icons as css class
##' m[,1] <- asIcon(c("up","up","down"))
##'  gtable(m, cont=g)
gtable <- function(items, multiple = FALSE, chosencol = 1,
                   icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL,
                   filter.FUN = NULL, handler = NULL, action = NULL,
                   container = NULL, ...,
                   width=200, height=200, ext.args=NULL,
                   paging = FALSE, 
                   col.widths = rep(20, ncol(as.data.frame(items)))
                   ) {

  tbl <- GTable$new(container$toplevel)
  tbl$init(items, multiple, chosencol, icon.FUN, handler, action, container,
           width=width, height=height, ext.args=ext.args, paging=paging, col.widths=col.widths)
  tbl
}


##' A class inherited by GTable and GDF
GWidgetGrid <- setRefClass("GWidgetGrid",
                           contains="ExtWidget",
                           fields=list(
                             store="ANY",
                             nms = "character" ## column name
                             ),
                           methods = list(
                             ## put common methods here
                             ## set__items is in subclass
                             get_items = function(i, j, ..., drop=TRUE) {
                               items <- store$proxy$get_data()
                               items[i,j, ..., drop=drop]
                             },
                             set_items = function(value, i, j, ...) {
                               if(missing(i) && missing(j)) {
                                 store$set_data(value)
                               } else {
                                 items <- store$get_data()
                                 items[i,j] <- value
                                 store$set_data(items)
                               }
                               nms <<- names(store$proxy$value)
                               store$load_data()
                             },
                             set_size = function(value, ...) {
                               "Set size of table (width,height) or columnWidths"
                               width <- height <- colWidths <- NULL
                               if(is.list(value)) {
                                 width <- value$width
                                 height <- value$height
                                 colWidths <- value$columnWidths
                               } else {
                                 width <- value[1]
                                 if(base:::length(value) >= 2) # need base!
                                   height <- value[2]
                               }
                               call_Ext("setWidth", width)
                               if(!is.null(height)) call_Ext("setHeight", height)
                               if(!is.null(colWidths))
                                 set_column_widths(colWidths)
                             },
                             dim = function() {
                               base:::dim(get_items())
                             },
                             len = function(x) {
                               "Length of data. For convenience, if passed an argument gives length of that"
                               if(missing(x))
                                 base:::length(get_items())
                               else
                                 base:::length(x)
                             },
                             ## Some column methods
                             call_column_method = function(meth, ...) {
                               "Call a method of the column model, like call_Ext"
                               l <- list(...)
                               out <- sapply(l, coerceToJSString)
                               cmd <- sprintf("%s.colModel.%s(%s);",
                                              get_id(),
                                              meth,
                                              paste(out, collapse=","))
                               cmd
                               add_js_queue(cmd)
                             },
                             set_column_name = function(value, column) {
                               call_column_method("setColumnHeader", column - 1, value)
                             },
                             get_names = function() {
                               nms
                             },
                             set_names = function(value) {
                               nms <<- value
                               sapply(seq_along(value), function(i) {
                                 set_column_name(value[i], i)
                               })
                             },
                             set_column_width = function(value, column) {
                               call_column_method("setColumnWidth", column - 1, value)
                             },
                             set_column_widths = function(value) {
                               sapply(seq_along(value), function(i) {
                                 set_column_width(value[i], i)
                               })
                             },
                             set_column_tooltip = function(value, column) {
                               "Set tooltop for specified column"
                               call_column_method("setColumnTooltip", column - 1, value)
                             },
                             set_column_tooltips = function(value) {
                               "Set tooltips for entire set of header columns"
                               sapply(seq_along(value), function(i) {
                                 set_column_tooltip(value[i], i)
                               })
                             },
                             ## handlers
                             add_handler_clicked = function(...) {
                               add_R_callback("cellclick", ...)
                             },
                             add_handler_double_click = function(...) {
                               add_R_callback("celldblclick", ...)
                             },
                             add_handler_column_clicked = function(...) {
                               add_R_callback("headerclick", ...)
                             },
                             add_handler_column_double_click = function(...) {
                              add_R_callback("headerdblclick", ...)
                             } 

                             ))

##' base class for gtable
##' @name gtable-class
GTable <- setRefClass("GTable",
                      contains="GWidgetGrid",
                      fields=list(
                        "store"="ANY",
                        "multiple"="logical",
                        "chosencol"="integer",
                        "paging" = "logical",
                        "page_size" = "integer"
                        ),
                      methods=list(
                        init=function(items, multiple, chosencol, icon.FUN, handler, action, container,...,
                          width=NULL, height=NULL, ext.args=NULL, paging=FALSE, col.widths) {

                          if(!is.data.frame(items))
                            items <- as.data.frame(items, stringsAsFactors=FALSE)

                          value <<- NA  # currently selected row(s) or NA
                          multiple <<- multiple
                          chosencol <<- chosencol
                          if(is.logical(paging)) {
                            paging <<- paging;
                            page_size <<- 25 ## override through assignment
                          } else {
                            paging <<- TRUE
                            page_size <<- paging
                          }

                          store <<- ExtArrayStore$new(container$toplevel)
                          store$init(items)
                          store$paging <<- paging
                          nms <<-names(items)
                            
                          constructor <<- "Ext.grid.GridPanel"
                          transport_signal <<- "cellclick"
                          
                          arg_list = list(
                            store=String(store$get_id()),
                            columns = String(store$proxy$make_column_model()),
                            stripeRows = TRUE,
                            enableRowBody = TRUE,
                            frame = FALSE,
                            autoExpandColumn=tail(names(items), n=1),
                            width=width,
                            height=height,
                            sm = String(ifelse(multiple,
                              'new Ext.grid.RowSelectionModel({singleSelect:false})',
                              'new Ext.grid.RowSelectionModel({singleSelect:true})'))
                            )
                          if(paging) {
                            store$page_size <<- page_size
                            paging_options <- list(
                                                   pageSize= page_size,
                                                   store= String(store$get_id()),
                                                   displayInfo=TRUE,
                                                   displayMsg= gettext("Displaying rows {0} - {1} of {2}"),
                                                   emptyMsg= gettext("No rows to display")
                                                   )
                            cmd <- sprintf("new Ext.PagingToolbar(%s)", toJSObject(paging_options))
                            arg_list[['bbar']] = String(cmd)
                          }
                          add_args(arg_list)

                          setup(container, handler, action, ext.args, ...)


                          ## set up paging
                          if(paging) { ## adjust size
                            cmd <- sprintf("%s.getTotalCount = function() {return %s};",
                                           store$get_id(), nrow(store$get_data()))
                            add_js_queue(cmd)
                          }

                          ## load data
                          store$load_data()

                        },

                        transport_fun = function() {
                          ## transport back row. Fine for multiple or not. Use id here, as sorting can
                          ## otherwise mess up relationship between index and data in R data frame
                          "var param={value: Ext.pluck(this.getSelectionModel().getSelections(),'id')}"
                        },
                        process_transport = function(value, ...) {
                          value <<- sort(value)
                        },
                        get_value = function(index=FALSE, drop=TRUE, ...) {
                          "Return selected value(s)"

                          if(length(value) ==1 && is.na(value))
                            return(NA)

                          
                          items <- store$get_data()

                          if(index)
                            return(value)
                          if(drop)
                            return(items[value, chosencol, drop=TRUE])
                          else
                            return(items[value, ,drop=FALSE])
                        },
                        set_value = function(value, index=TRUE, ...) {
                          ## Value is index if numeric and index is TRUE
                          ## value is logical if index is trye and logical
                          ## value is matched against names
                          if(index) {
                            if(is.logical(value))
                              value <<- which(value)
                            else
                              value <<- value
                          } else {
                            value <<- match(value, get_items(j=chosencol))
                          }

                          "Set value where value is row index to select"
                          if(base:::length(value) == 0 ||
                             (base:::length(value) == 1 && is.na(value)) ||
                             value[1] <= 0) {
                                        # clear out
                            cmd <- paste(sprintf("var sm = %s.getSelectionModel();", get_id()),
                                         "sm.clearSelections();",
                                         sep="")
                          } else {
                            cmd <- paste(sprintf("var sm = %s.getSelectionModel();", get_id()),
                                         sprintf("sm.selectRows(%s);", toJSArray(value -1)),
                                         sep="")
                          }
                          add_js_queue(cmd)
                        },
                      
                        
                        set_items = function(value, i, j, ...) {
                          callSuper(value, i, j, ...)
                          
                          if(paging) {
                            ## need to notify grid that the total
                            ## count has increased or decreased. This
                            ## is done thorugh the getTotalCount JS
                            ## function
                            cmd <- paste(sprintf("%s.getTotalCount = function() {return %s}",
                                                 store$get_id(), nrow(store$get_data())),
                                         sep="")
                          add_js_queue(cmd)
                          } else {
#                            cmd <- sprintf("%s.getUpdater().update(%s)",
#                                           get_id(),
#                                           toJSObject(store$proxy$get_url_list())
                            ## cmd <- sprintf("%s.doRequest(%s_);",
                            ##                store$get_id(),
                            ##                toJSObject(store$proxy$get_url_list()))
                            ## 
                          }

                        },
                        filter = function(colname, regex) {
                          "Use filter by regular expression. No visible<- method to adjust visible rows"
                          if(missing(colname) || is.na(match(colname, names(store$get_data()))))
                            return()

                          if(missing(regex) || nchar(regex) == 0) {
                            cmd <- sprintf("%s.clearFilter();", store$get_id())
                          } else {
                            cmd <- sprintf("%s.filter(%s, RegExp('%s'));",
                                           store$get_id(),
                                           ourQuote(colname),
                                           regex)
                          }
                          add_js_queue(cmd)
                        },
                        add_handler_changed = function(...) {
                          add_handler_clicked(...)                          
                        }
                        ))