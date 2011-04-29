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

##' @include ext-component.R
NA

##' Base class for Ext widgets
ExtWidget <- setRefClass("ExtWidget",
                         contains="ExtComponent",
                         methods = list(
                           init = function(...) {
                             "Initialize widget, including writing constructor"
                           },
                           setup = function(container, handler, action=NULL, ext.args=NULL, ...) {
                             "Set up widget"

                             if(!is.null(ext.args))
                               args$extend(ext.args)

                             container$add_dots(.self, ...)
                             write_constructor()
                             container$add(.self, ...)

                             if(length(nchar(transport_signal))) # character(0) or not?
                               write_transport()
                             
                             if(!missing(handler)  & !is.null(handler))
                               add_handler_changed(handler, action)
                           },
                           get_value = function(...) {
                             "get main property for widget"
                             value
                           },
                           set_value = function(value, ...) {
                             "Set main property for widget"
                             value <<- value
                             ## in subclass call setText or some such...
                           },

                           ## basic Ext methods
                           destroy = function() {
                             "destroy component"
                             call_Ext("destroy")
                           },
                           len = function(x) {
                             "Give a length method as default"
                             if(!missing(x))
                               base:::length(x)
                           },
                           set_enabled = function(value) {
                             "Disable/enable component"
                             if(value)
                               call_Ext("enable")
                             else
                               call_Ext("disable")
                           },
                             
                           set_focus = function() {
                             "focus component"
                             call_Ext("focus")
                           },
                           set_font = function(value) {
                             "Set font"
                             ## XXX no default implementation
                           },
                           set_visible = function(bool) {
                             "Change visibility"
                             call_Ext("setVisible", list(visible=as.logical(bool)))
                           },
                           set_size = function(val) {
                             "Set size, specified as width or c(width, height)"
                             if(is.list(val))
                               val <- unlist(val)
                             
                             if(length(val) == 1)
                               call_Ext("setWidth", val)
                             else
                               call_Ext("setSize", val[1], val[2])
                           },
                           set_tooltip = function(tip) {
                             "Set tooltip for widget"
                             call_Ext("setTooltip", tip)
                           },
                           ## Handlers. XXX add more
                           add_handler_changed = function(...) {
                             "Must be defined for each widget, no default"
                           }

                           ))
                         
                           
                             
                           

                             
                             
