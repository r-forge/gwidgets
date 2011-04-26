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


##' Basic button widget
##'
##' A button responds to mouse clicks by calling its handler
##'
##' @param text button text
##' @param border ignored
##' @param handler callback to call when button is clicked
##' @param action passed to callback as \code{h$action}
##' @param container parent container
##' @param ... passed to parent container's \code{add} method
##' @param width width of button, if desired
##' @param height height of button, if desired
##' @param ext.args optional list of configuration options for ext
##' widgets. For example \code{style=list(padding='20px')}.
##' @return a \code{GButton} instance
##' @export
##' @examples
##' w  <- gwindow("test")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' b <- gbutton("click me", cont=g, handler=function(h,...) {
##'   galert("hello world", parent=w)
##' })
##' ## has an icon
##' b <- gbutton("up", cont=g)
##' ## just an icon
##' b <- gbutton("help", cont=g); b$call_Ext("setText")
##' ## an action
##' a <- gaction("help", parent=w, handler=function(h,...) {
##'  galert("action", parent=w)
##' })
##' b <- gbutton(action=a, cont=g)
gbutton <- function(text="",
                    border=TRUE,
                    handler=NULL,
                    action=NULL,
                    container, ...,
                    width=NULL,
                    height=NULL,
                    ext.args=NULL
                    ) {
  b <- GButton$new(toplevel=container$toplevel)
  b$init(text, border, handler, action, container, ..., width=width, height=height, ext.args=ext.args)
  return(b)
}

##' class for gbutton
##' @name gbutton-class
GButton <- setRefClass("GButton",
                       contains="ExtWidget",
                       fields=list(
                         stub="ANY"
                         ),
                       methods=list(
                         init = function(text="", border=TRUE,
                           handler=NULL, action=NULL, container, ...,
                           width=NULL, height=NULL, ext.args=NULL
                           ) {

                           if(!is.null(action) && is(action,"GAction"))
                             return(init_action(action, container,..))
                           
                           value <<- text
                           
                           constructor <<- "Ext.Button"
                           arg_list <- list(
                                            tooltip = getFromDots("tooltip", ...),
                                            width = width,
                                            height = height
                                            )
                           
                           add_args(arg_list)
                           
                           if(!is.null(ext.args))
                             args$extend(ext.args)

                           container$add_dots(.self, ...)

                           write_constructor()
                           add_details(container, handler, action)

                           set_value(text)
                         },
                         init_action = function(action, container, ...) {
                           "Initialize widget if it is an action item"
                           cmd <- sprintf("var %s = new Ext.Button(%s)", get_id(), action$get_id())
                           add_js_queue(cmd)
                           container$add(.self, ...)
                         },
                         ## main property
                         get_value = function() {
                           "Return label"
                           value
                         },
                         
                         set_value = function(text, ...) {
                           "Set label"
                           value <<- text
                           call_Ext("setText", text)
                           u <- getStockIconByName(text)
                           call_Ext("setIconClass", getWithDefault(u, ""))
                         },
                         
                         ##
                         set_icon = function(icon) {
                           "Set icon"
                           call_Ext("setIcon", icon)
                         },
                         set_tooltip = function(tip) {
                           call_Ext("setTooltip", tip)
                         },
                         ##
                         add_handler_changed = function(...) {
                           "Change handler is button click"
                           add_handler_clicked(...)
                         }
                         )
                       )
                       
                         
