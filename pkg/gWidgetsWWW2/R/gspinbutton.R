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

##' @include gslider.R
NA

##' Basic spinbutton
##'
##' @param from from value
##' @param to to
##' @param by by. From to by are same as seq() usage
##' @param value initial value
##' @param handler handler for when change is made
##' @param action passed to the handler, if given
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @param width width, ignored
##' @param height height, ignored
##' @param ext.args list. Can pass in other configuration arguments to Ext widget
##' @return an ExtWidget instance
##' @note Buggy! For some reason both trigger icons don't show! Just the down arrow!
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' sp <- gspinbutton(cont=w)
gspinbutton <- function(from = 0, to = 100, by = 1, value = from,
                        handler = NULL, action = NULL, container = NULL, ...,
                        width=NULL, height=NULL, ext.args=NULL
                        ) {

  sp <- GSpinbutton$new(container$toplevel)
  sp$init(from, to, by, value,  handler, action, container, ...,
          width=width, height=height, ext.args=ext.args)
  sp
}

##' base class for gspinbutton
##' @name gspinbutton-class
GSpinbutton <- setRefClass("GSpinbutton", 
                       contains="GSlider",
                       fields=list(
                         coerce.with="function"
                         ),
                       methods=list(
                         init=function(from, to, by, value,  handler, action, container, ...,
                           coerce.with=as.numeric,
                           width, height, ext.args) {
                           value <<- value

                            
                           constructor <<- "Ext.ux.form.SpinnerField"
                           transport_signal <<- "change"

                           arg_list =list(
                             minValue=from,
                             maxValue=to,
                             incrementValue=by,
                             value=value,
                             accelerate=TRUE
                             )
                           add_args(arg_list)

                           if(!is.null(ext.args))
                             args$extend(ext.args)
                           
                           container$add_dots(.self, ...)                           

                           write_constructor()
                           add_details(container, handler, action)
                           
                           .self

                         },
                         get_value = function(...) value,
                         set_value = function(value, ...) {
                           value <<- value                           

                           call_Ext("setValue", value)
                         },
                         get_items = function(...) items,
                         set_items = function(items, ...) {
                           ## XXX No methods in extjs to set the values (minValue, maxValue, increment)
                           ### after construction so we can't implement [<- method
                         },
                         transport_fun = function() {
                           "param={value: newValue}"
                         },
                         process_transport = function(value) {
                           value <<- as.numeric(value)
                         },
                         ##
                         add_handler_changed = function(...) {
                           "Change handler when slider moves"
                           add_handler_change(...)
                         }
                         )
                       )
