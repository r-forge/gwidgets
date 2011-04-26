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

##' Base class for containers, which are derived from Ext.Panel
ExtContainer <- setRefClass("ExtContainer",
                            contains="ExtComponent",
                            fields=list(
                              children="Array",
                              spacing = "ANY"
                              ),
                            methods=list(
                              initialize=function(...) {
                                children <<- Array$new()
                                
                                callSuper(...)
                              },

                              add = function(child, expand=NULL, anchor=NULL, fill=NULL, ...) {
                                children$push(child, child$get_id()) # keep our children
                                call_Ext("add", String(child$get_id())) # add to GUI
                                call_Ext("doLayout") # inefficient, but whatever
                              },
                              insert = function(child, index,  expand=NULL, anchor=NULL, fill=NULL, ...) {
                                "Insert child at index (1-based)"
                                children$push(child, child$get_id())
                                
                                call_Ext("insert", index - 1, child$get_id())
                                call_Ext("doLayout") 
                              },
                              delete = function(child, ...) {
                                "Remove child from container"
                                children$remove_by_name(child$get_id())
                                call_Ext("remove", child$get_id())
                              },
                              add_dots = function(child, ...) {
                                "Process add arguments from ... expand, anchor, fill and spacing"
                                ## Must call before add, add these take effect when child constructor is
                                ## written which happens before add.
                                
                                ## spacing first
                                if(is.numeric(spacing))
                                  child$add_args(list(style=list(padding=sprintf("%spx",spacing))))
                                else if(is.character(spacing) && nchar(spacing))
                                  ## eg spacing="'5px,0px,0px,5px'"
                                  child$add_args(list(style=list(padding=spacing)))

                                ## expand, anchor fill
                                ## expand
                                expand <- getFromDots("expand", ..., default=NULL)
                                if(!is.null(expand) && expand)  
                                  child$add_args(list(flex=3))
                                
                                ## fill. isn't working
                                fill <- getFromDots("fill", ..., default=NULL)
                                if(!is.null(fill)) {
                                  if(fill == "x")
                                    child$add_args(list(width="auto"))
                                  if(fill == "y")
                                    child$add_args(list(height="auto"))
                                  else
                                    child$add_args(list(width="auto", height="auto"))
                                }



                              },
                              
                              set_enabled = function(value, ...) {
                                "Recursively enable/disable child components"
                                if(value)
                                  call_Ext("cascade", String("function(){ this.enable()}"))
                                else
                                  call_Ext("cascade", String("function(){ this.disable()}"))
                              },
                              set_focus = function() {
                                "Call focus on container"
                                call_Ext("focus")
                              },

                              get_height = function() {
                                "Return height. Doesn't work"
                                call_Ext("getHeight")
                              },
                              set_height = function(px) {
                                "Set height in pixels"
                                call_Ext("setHeight", px)
                              },
                              get_width = function() {
                                "Return width. XXX Won't work! doesn't come back to R"
                                call_Ext("getWidth")
                              },
                              set_width = function(px) {
                                "Set width in pixels"
                                call_Ext("setWidth", px)
                              },
                              set_size = function(value) {
                                "w is c(width, height)"
                                if(length(value) == 1)
                                  set_width(value[1])
                                else
                                  call_Ext("setSize", value[1], value[2])
                              },
                              hide = function() {
                                "Hide container"
                                call_Ext("hide")
                              },
                              show = function() {
                                "show container"
                                call_Ext("show")
                              },
                              set_visible = function(value) {
                                "Show container and its siblings"
                                if(value)
                                  show()
                                else
                                  hide()
                              },
                              mapAnchorToCSSClass = function(anchor) {
                                "Return a css class for the anchor value"
                                if(is.null(anchor))
                                  return("td-northwest")
                                if(all(anchor == 0))
                                  return("td-center")
                                
                                lr <- c("west", "", "east")
                                ns <- c("north", "", "south")
                                m <- rbind(paste("td", "-", ns[1], lr, sep=""),
                                           paste("td", "-", ns[2], lr, sep=""),
                                           paste("td", "-", ns[3], lr, sep="")
                                           )
                                
                                m[ 2 - anchor[2], 2 + anchor[1]]
                              }
                              

                              ## XXX could put in some handlers here ...

                              ))
   
