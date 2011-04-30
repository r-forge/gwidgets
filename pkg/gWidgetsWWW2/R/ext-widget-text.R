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

##' Base class for text widgets
ExtWidgetText <- setRefClass("ExtWidgetText",
                             contains="ExtWidget",
                             fields=list(
                               stub = "ANY"
                               ),
                             methods=list(
                               set_value = function(value, ...) {
                                 "set text"
                                 value <<- value
                                 call_Ext("setValue", value)
                               },
                               transport_fun = function() {
                                 "param = {value: this.getValue()};"
                               },
                               add_handler_changed = function(handler, action=NULL) {
                                 "Called when we lose focus"
                                 add_R_callback("change", handler, action)
                               },
                               add_handler_keystroke = function(handler, action=NULL) {
                                 add_R_callback("keyup", handler, action,
                                                extra = "var param = Ext.util.JSON.encode({key:e.getKey()})")
                                                
                               }
                               ))
