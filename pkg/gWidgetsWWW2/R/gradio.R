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

##' Radio button group
##'
##' A basic radio button group. Specify the labels through items. The main value is the label or index
##' @param items Items to choose from
##' @param selected index of initially selected item
##' @param horizontal logical. Horizontal or vertical layout. (See also columns)
##' @param handler called when radio buttons change
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of container
##' @param width width in pixels
##' @param height height in pixels
##' @param ext.args list. Can pass in additional ext arguments, overriding defaults
##' @param columns Can be used to override horizontal TRUE or FALSE
##' @return an ExtWidget object
##' @export
##' @note the \code{[<-} method (to change the labels) is not implemented
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' rb <- gradio(state.name[1:4], cont=w)
##' addHandlerChanged(rb, handler=function(h,...) {
##'   galert(paste("You clicked", svalue(h$obj)), parent=w)
##' })
##' svalue(rb, index=TRUE) <- 1 # by index
##' svalue(rb) <- state.name[2] # by value
gradio <- function(items,
                   selected = 1, horizontal=FALSE,
                   handler = NULL, action = NULL, container = NULL, ...,
                   width=NULL, height=NULL, ext.args=NULL, columns=ifelse(horizontal,1,length(items))) {

  rb <- GRadio$new(container$toplevel)
  rb$init(items,
             selected, horizontal,
             handler, action, container, ...,
             width=width, height=width, ext.args=ext.args, columns=columns)
}

##' Base class for gradio
##' @note TODO No way to change number of radio buttons via [<- ir get/set_items, as of now
##' @name gradio-class
GRadio <- setRefClass("GRadio",
                       contains="ExtWidget",
                       fields=list(
                         items="ANY"
                         ),
                       methods=list(
                         init = function(items,
                           selected = 1, horizontal=FALSE,
                           handler = NULL, action = NULL, container = NULL, ...,
                           width=NULL, height=NULL, ext.args=NULL, columns=ifelse(horizontal,1,length(items))) {
                           
                           value <<- selected # value is index
                           items <<- items
                           
                           constructor <<- "Ext.form.RadioGroup"
                           transport_signal <<- "change"
                           
                           ##callSuper(list(), toplevel = container$toplevel)

                           ## this is a bit different, as we
                           ## need to define .self (via callSuper) before args here to get the id
                           arg_list <- list(items=String(items_as_array()),
                                            width = width,
                                            height = height,
                                            columns=columns, vertical=!horizontal
                                            )
                           
                           add_args(arg_list)

                           setup(container, handler, action, ext.args, ...)
                           
                           set_value(selected, index=TRUE)
                           .self
                         },
                         ## main property. We store the index in value, not the label
                         get_value = function(index=FALSE, ...) {
                           "Return index"
                           if(index)
                             value
                           else
                             items[value]
                         },
                         set_value = function(value, index=FALSE, ...) {
                           "Set value. Default is by value, not index"
                           if(index)
                             value <<- value
                           else
                             value <<- match(value, items)

                           ## bit awkward to set the value
                           if(is.na(.self$value)) {
                             return()
                           }
                           
                           cmd <- sprintf("%s.items.get(%s).setValue(true)",
                                          get_id(),
                                          .self$value - 1) # 0-based arrays
                           add_js_queue(cmd)
                         },
                         get_items = function(...) {
                           items
                         },
                         set_items = function(items, ...) {
                           "Set items, update GUI"
                           items <<- items
                           ## XXX update radio buttons??? TODO
                         },
                         ## transport, brings back index as string
                         transport_fun = function() {
                           "param =  {value: newValue.getRawValue()};"
                         },
                         process_transport = function(value) {
                           ind <- as.numeric(value)
                           value <<- ind
                         },
                         ##
                         items_as_array = function() {
                           "Return items as array"
                           makeRadio <- function(label, i,  name) {
                             ## inputValue is 1-based index
                             sprintf("new Ext.form.Radio({boxLabel:'%s', inputValue: %s, name:'%s'})", label, i, name)
                           }
                           buttons <- mapply(makeRadio, items, seq_along(items), rep(.self$get_id(), len=length(items)))

                           out <- paste("[",
                                        paste(buttons, collapse=","),
                                        "]", sep="")
                           return(out)
                           
                         },
                         ##
                         add_handler_changed = function(...) {
                           "Change handler is when radio button changes, perhaps through a click"
                           add_handler_clicked(...)
                         },
                         add_handler_clicked = function(...) {
                           "Click here is change -- perhaps through some method call, not just a moust event"
                           add_handler_change(...)
                         }
                         )
                       )
                       
                         