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

##' base class for ext containers and widgets.
ExtComponent <- setRefClass("ExtComponent",
                            contains="ExtObject",
                            fields=list(constructor="character"),
                            methods=list(
                              ## some generic handlers
                              add_handler_blur = function(...) {
                                "Call when widget loses focus"
                                add_R_callback("blur", ...)
                              },
                              add_handler_change = function(...) {
                                "A 'change' handler' -- not add_handler_changed which maps to a generic"
                                add_R_callback("change", ...)
                              },
                              add_handler_clicked = function(...) {
                                "When object is clicked"
                                add_R_callback("click", ...)
                              },
                              add_handler_doubleclick = function(...) {
                                add_R_callback("celldblclick", ...)
                              },
                              add_handler_idle = function(interval, handler, action=NULL, ...) {
                                "Add a callback after an interval"
                                cbid <- toplevel$add_R_handler(.self, handler, action=action, ...)
                                task <- sprintf("%s_%s_task", get_id(), cbid)
                                cmd <- paste(sprintf("var %s {", task),
                                             sprintf("run: function() callRhandler(%s),", cbid),
                                             sprintf("interval : %s", interval),
                                             "};",
                                             sprintf("(new Ext.util.TaskRunner()).start(%s);",
                                                     task),
                                             sep="")
                                add_js_queue(cmd)
                                out <- list(cbid=cbid, task=task)
                                class(out) <- c("idleHandler", class(out))
                                out
                              },
                              remove_idle_handler = function(cbid) {
                                cmd <- sprintf("(new Ext.util.TaskRunner()).stop(%s);", cbid$task)
                                add_js_queue(cmd)
                              }
                              
                              )
                            )
