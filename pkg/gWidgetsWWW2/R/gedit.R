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


##' gedit widget
##'
##' No [<- method. This can be done with a combobox though.
##' @param text initial text
##' @param width width in characters. Converted to pixels by multiplying by 8.
##' @param coerce.with Function to call for coercion from text. If no
##' coercion be careful when using the values, as the user can potentiall type in malicious things.
##' @param handler Change handler. Change is a "blur" event (when widget loses focus) and when key is activated.
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param ext.args extra arguments to pass to constructor
##' @return an ExtWidget object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' e <- gedit("initial text", cont=w)
##' addHandlerChanged(e, handler=function(h,...) {
##' galert(paste("You entered", svalue(h$obj)), parent=w)
##' })
gedit <- function (text = "", width = 25, coerce.with = NULL,
                   handler = NULL,  action = NULL, container = NULL, ...,
                   ext.args=NULL) {

  e <- GEdit$new(container$toplevel)
  e$init(text, width, coerce.with, handler, action, container, ..., ext.args=ext.args)
  return(e)
}



##' base class for gedit
##' @name gedit-class
GEdit <- setRefClass("GEdit",
                     contains="ExtWidgetText",
                     fields=list(
                       coerce.with="ANY"
                       ),
                     methods=list(
                       init=function(text = "", width = 25, coerce.with = NULL,
                         handler = NULL,  action = NULL, container = NULL, ...,
                         ext.args=NULL
                         ) {
                         
                         value <<- text
                         coerce.with <<- coerce.with
                         constructor <<- "Ext.form.TextField"
                         transport_signal <<- "keypress"
                         
                         ## constructor arguments
                         arg_list <- list(value = text,
                                          enableKeyEvents=TRUE,
                                          width = ifelse(is.character(width), width, sprintf("%spx", 8*width))
                                          )
                         add_args(arg_list)

                         setup(container, handler, action, ext.args, ...)
                         
                         .self
                       },
                       transport_fun = function() {
                         "var param = {value: w.getValue()}"
                       }
                       ))
                     
