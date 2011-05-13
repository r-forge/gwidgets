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

##' @include ext-container.R
NA

##' Notebook container
##'
##' @param tab.pos where to place tabs. A value of 1 is the bottom, else the top.
##' @param close.buttons Logical. Are there close buttons on the tabs
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param width width in pixels, if specified
##' @param height height in pixels, if specified
##' @param ext.args extra arguments to pass to Ext constructor
##' @return an ExtContainer object
##' @export
##' @examples
##' w <- gwindow()
##' nb <- gnotebook(cont=w)
##' gbutton("hello", container=nb, label="My label") ## pass in label argument through ... to \code{add}
gnotebook <- function(tab.pos = 3, close.buttons = FALSE, container, ...,
                      width=NULL, height=NULL, ext.args=NULL
                      ) {
  nb <- GNotebook$new(container$toplevel)
  nb$init(tab.pos, close.buttons, container, ..., width=width, height=height, ext.args=ext.args)
  nb
}

##' base class for gnotebook
##' @name gnotebook-class
GNotebook <- setRefClass("GNotebook",
                         contains="ExtContainer",
                         fields=list(
                           "notebook_children"="list",
                           "closable"="logical"
                           ),
                         methods=list(
                           init=function(tab.pos=3, close.buttons=FALSE, container, ...,
                             width=NULL, height=NULL, ext.args=NULL) {

                             notebook_children <<- list()
                             constructor <<- "Ext.TabPanel"
                             value <<- 1 ## track through
                             closable <<- close.buttons
                             
                             transport_signal <<- "tabchange"
                             
                             arg_list <- list(tabPosition = ifelse(tab.pos==1, "bottom", "top"),
                                              frame = TRUE,
                                              activeTab = .self$value - 1,
                                              enableTabScroll = TRUE,
                                              defaults=list(autoScroll=TRUE),
                                              width=width,
                                              height=height
                                              )
                             add_args(arg_list)
                             
                             if(!is.null(ext.args))
                               args$extend(ext.args)
                             
                             container$add_dots(.self, ...)                           

                             write_constructor()
                             write_transport()

#                             add_R_callback("destroyed", handler=function(h,...) {
#                               h$obj$dispose(h$value)
#                             }, param_defn ="{value:this.items.indexOf(c) + 1}")
                             
                             container$add(.self, ...)
                             
                             .self
                             
                           },
                           transport_fun = function() {
                             "param={value: this.items.indexOf(tab) + 1}" # id, not index
                           },
                           process_transport = function(value) {
                             value <<- value
                           },
                           add = function(child, label="tab", tooltip=NULL, ...) {
                             "Add child. Label is tab label"

                             ## book keep
                             children$push(child, child$get_id())
                             notebook_children[[child$id]] <<- child

                             
                             ## store name in child. XXX Does this work?
                             child$set_attr("label", label)
                             
                             call_Ext("add", list(title=label,
                                                  closable=closable,
                                                  tabTip = tooltip,
                                                  items=String(sprintf("['%s']", child$id))
                                                  ))
                             call_Ext("setActiveTab", value - 1)
                             call_Ext("doLayout")
                             
                             toplevel$do_layout()
                           },
                           dispose = function(index) {
                             "For deleting. Index can be numeric or character"
                             if(missing(index))
                               index <- length() - 1L# last one

                             if(is.character(index))
                               index <- match(index, names(notebook_children))

                             notebook_children[[index]] <<- NULL
                             cmd <- paste(sprintf("var tab = %s.getComponent(%s);", get_id(), index-1),
                                          sprintf("%s.remove(tab);", get_id()),
                                          sep="")
                             add_js_queue(cmd)
                           },
                           len = function() {
                             "Number of tabs"
                             base:::length(notebook_children)
                           },
                           get_value = function() {
                             value
                           },
                           set_value = function(value) {
                             "make tab value visible"
                             value <<- value
                             call_Ext("setActiveTab", value - 1)
                           },
                           get_names = function() {
                             "Return tab names"
                             sapply(notebook_children, function(i) i$get_attr("label"))
                           },
                           set_names = function(value) {
                             "Set tab names"
                             sapply(seq_along(notebook_children), function(i) {
                               child <- notebook_children[[i]]
                               child$set_attr("label",value[i])
                               cmd <- paste(sprintf("var tab = %s.getComponent(%s);", get_id(), i-1),
                                            sprintf("tab.setTitle(%s);", ourQuote(value[i])),
                                            sep="")
                               add_js_queue(cmd)
                             })
                           }

                           ))
                           
                        