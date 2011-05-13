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

##' checkbox widget
##' 
##' @param text character. text label for checkbox. 
##' @param checked logical. initial state (Set later with \code{svalue<-})
##' @param use.togglebutton logical. XXX not implemented If TRUE, represent with a togglebutton, else use check box 
##' @param handler handler called when state is toggled. Check value
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container.
##' @param width width of widget. May be necessary, otherwise may take all horizontal real estate
##' @param height height of widget (px)
##' @param ext.args list of optional argument for ExtJS constructor
##' @export
##' @note No method to set label
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' cb <- gcheckbox("Check me?", cont=w, handler=function(h,...) if(svalue(h$obj)) galert("checked", parent=w))
gcheckbox = function(text="", checked = FALSE, use.togglebutton=FALSE,
  handler = NULL, action = NULL,  container = NULL,...,
  width=NULL, height=NULL, ext.args=NULL) {

  cb <- GCheckbox$new(container$toplevel)
  cb$init(text, checked, use.togglebutton, handler, action, container, ...,
                  width=width, height=height, ext.args=ext.args)
  return(cb)
  }


##' A group of checkboxes
##' 
##' @param items vector of items to select from
##' @param checked initial value of checked state. Recycled
##' @param horizontal Layout horizontally?
##' @param use.table Needs implementing. If TRUE, uses a grid widget with checkboxes to
##' display. If TRUE, horizontal is ignored, and items may be a data
##' frame.
##' @param handler handler called when state changes
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of container
##' @param width width of widget. May be necessary, otherwise may take all the horizontal real estate
##' @param height height of widget (px)
##' @param ext.args list of optional argument for ExtJS constructor
##' @return An ExtWidget instance
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' cbg <- gcheckboxgroup(state.name[1:4], cont=w)
gcheckboxgroup = function (items, checked = FALSE, horizontal = FALSE, use.table=FALSE,
  handler = NULL, action = NULL,
  container = NULL, ...,
  width=NULL, height=NULL, ext.args=NULL) {
  
  cb <- GCheckboxGroup$new(container$toplevel)
  cb$init(items, checked, horizontal, use.table, handler, action, container, ...,
                     width=width, height=height, ext.args=ext.args)
  return(cb)
}



##' Base class for checkbox group
##' @note TODO share code with gradio -- one should be a subclass Gradio - GCheckboxGroup - GCheckbox
##' @name gcheckboxgroup-class
GCheckboxGroup <- setRefClass("GCheckboxGroup",
                         contains="ExtWidget",
                         fields=list(
                           items="ANY"
                         ),
                       methods=list(
                         init = function(items,
                           checked = FALSE, horizontal=FALSE, use.table=FALSE,
                           handler = NULL, action = NULL, container = NULL, ...,
                           width=NULL, height=NULL, ext.args=NULL, columns=ifelse(!horizontal,1,length(items))) {
                           
                           value <<- checked # value is index
                           items <<- items
                           
                           constructor <<- "Ext.form.CheckboxGroup"
                           transport_signal <<- "change"
                           
                           arg_list <- list(items=String(items_as_array()),
                                            autoWidth=FALSE,
                                            width = width,
                                            height = height,
                                            columns=columns, vertical=!horizontal
                                            )
                           add_args(arg_list)

                           setup(container, handler, action, ext.args, ...)
                           
                           set_value(as.logical(rep(checked, len=length(items))), index=FALSE)
                           .self
                         },
                         ## main property. We store the index in value, not the label
                         get_value = function(index=FALSE) {
                           "Return index"
                           if(index)
                             value
                           else
                             items[value]
                         },
                         set_value = function(value, index=TRUE) {
                           "Set value. Value may be index, logical, or labels"
                           if(index)
                             value <<- value
                           else if(is.logical(value))
                             value <<- which(value)
                           else
                             value <<- match(value, items)

                           value <<- value[!is.na(.self$value)]
                           
                           tmp <- rep(FALSE, len=length(items))
                           if(length(value))
                             tmp[value] <- TRUE
                           call_Ext("setValue", String(toJSArray(tmp)))
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
                           paste("var x = []; Ext.each(this.getValue(), function(val) {x.push(val.getRawValue())});",
                                 "param =  {value: x};", sep="")
                         },
                         process_transport = function(value) {
                           ## coerce to numeric, that's about it
                           ind <- as.numeric(value)
                           value <<- ind
                         },
                         ##
                         items_as_array = function() {
                           "Return items as array"
                           makeCheck <- function(label, i,  name) {
                             ## inputValue is 1-based index
                             sprintf("new Ext.form.Checkbox({boxLabel:'%s', inputValue: %s, name:'%s'})", label, i, name)
                           }
                           buttons <- mapply(makeCheck, items, seq_along(items), rep(.self$get_id(), len=length(items)))

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
                       
                         
##' Base class for gcheckbox
##' @name gcheckbox-class
GCheckbox <- setRefClass("GCheckbox",
                         contains="GCheckboxGroup",
                         method=list(
                           init=function(text, checked=FALSE,
                             use.togglebutton=FALSE,
                             handler=NULL, action=NULL, container=NULL, ...,
                             width=NULL, height=NULL, ext.args=NULL) {

                             value <<- if(checked) 1 else numeric(0)
                             items <<- text
                           
                             constructor <<- "Ext.form.CheckboxGroup"
                             transport_signal <<- "change"

                             arg_list <- list(items=String(items_as_array()),
                                              autoWidth=FALSE,
                                              width = width,
                                              height = height
                                              )
                             add_args(arg_list)

                             setup(container, handler, action, ext.args, ...)
                             
                             set_value(as.logical(rep(checked, len=length(items))), index=FALSE)
                             .self
                             
                           },
                           get_value = function(index=TRUE) {
                             "Return a logical value (if checked). Can get label if index=FALSE"
                             if(index) {
                               return(1 %in% value) # return logical
                             } else {
                               items[1] # the label
                             }
                           },
                           set_value = function(value, ...) {
                             "Set value. Value is logical TRUE or FALSE"
                             if(as.logical(value)) {
                               callSuper(value=1, index=TRUE)
                             }
                           }
                           ))
                         