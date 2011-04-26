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

##' @include ext-base.R
NA

##' gaction implementation
##'
##' actions are reusable encapsulations of actions. The
##' \code{svalue<-} method can be used to update the text associated
##' with an action. Use \code{enabled<-} to adjust sensitivity to
##' mouse events. The \code{visible<-} method can adjust if objects
##' proxying the action are visible. The \code{set_icon} reference
##' class method can be used to set the icon (no S3 method)
##' @param label Text for action
##' @param tooltip tooltip. Ignored
##' @param icon action icon class
##' @param handler handler called when action activated
##' @param parent toplevel window of action (where it can be
##' called). Required here
##' @param ... ignored
##' @return an \code{ExtWidget} object
##' @export
##' @examples
##' w <- gwindow()
##' a <- gaction("some action", handler=function(h,...) galert("asdf", parent=w), parent=w)
##' b <- gbutton(action=a, cont=w)
##' enabled(a) <- FALSE
##' svalue(a) <- "new text"
##' #
gaction <- function(label, tooltip=label, icon=NULL, handler, parent, ...) {
  a <- GAction$new(parent$toplevel)
  a$init(label, tooltip, icon, handler, parent, ...)
  a
}

##' Class for gaction
##' @rdname gaction-class
GAction <- setRefClass("GAction",
                       contains="ExtObject",
                       fields=list(
                         handler_id = "character"
                         ),
                       method=list(
                         init = function(label, tooltip=label, icon=NULL, handler, parent, ...) {

                           handler_id <<- add_R_callback("action", handler)
                           value <<- label
                           
                           fn <- sprintf("function() {callRhandler(%s)}",
                                         .self$handler_id)
                           
                           constructor <<- "Ext.Action"
                           arg_list <- list(id=NULL,
                                            itemId=.self$id,
                                            handler=String(fn)
                                            )
                           add_args(arg_list)
                           write_constructor()

                           set_value(label)
                           if(is.null(icon)) {
                             ## match the label?
                             icon <- getStockIconByName(label)
                           }
                           set_icon(icon)
                             
                           
                         },
                         get_value = function() value,
                         set_value = function(value, ...) {
                           value <<- value
                           call_Ext("setText", value)
                         },
                         set_enabled = function(value) {
                           "Set whether widget is enabled"
                           if(value)
                             call_Ext("enable")
                           else
                             call_Ext("disable")
                         },
                         set_visible = function(value) {
                           "Set whether widgets proxying action are visible"
                           callExt("setHidden", value)
                         },
                         set_icon = function(value) {
                           "Set icon class for action item"
                           if(!is.null(value)) 
                             call_Ext("setIconClass", value)
                         }
                         ))
                         
