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

##' gtree widget
##'
##' Widget to display heirarchical data. The data is described by a
##' function which returns the offspring for agiven node in a specific
##' format. This implementation can only show one column's worth of
##' data and an icon.
##' @param offspring Function with signature (path, data). Returns a
##' data frame with columns: id (which create the path), hasOffspring
##' (logical indicating if it has children), optionally an icon (a
##' stock icon name) and a value, which defaults to the id. The id's
##' must not have a ':' as that is chosen as a delimiter.
##' @param offspring.data passed to offspring call so that the
##' offspring function can be parameterized if desired.
##' @param icon.FUN NULL, Ignored. Place stock icons names in third column through offspring function, or \code{NULL}
##' @param chosencol (if/when?) grid is implemented will work with svalue method
##' @param multiple for multiple selection (not implemented)
##' @param handler called on double click
##' @param action passed to handler
##' @param container container object
##' @param ... passed to containers add method
##' @param width width in pixels
##' @param height height in pixels
##' @param ext.args extra configuration arguments passed to Ext constructor
##' @note  TODO: implement multiple slection, grids,
##' @export
##' @examples
##' # galton watson
##' offspring <- function(path, ...) {
##'   x <- rbinom(2, 1, p=1/2)
##'   icons <- c("dismiss","ok")[2-x]
##'   nms <- c("branch","leaf")[x+1]
##'   ttip <- paste("This is a ", nms)
##'   data.frame(id=letters[1:2], hasoffspring=as.logical(x), icons=icons, value=nms, qtip=ttip, stringsAsFactors=FALSE)
##' }
##' w <- gwindow("Galton Watson tree")
##' g <- ggroup(cont=w, horizontal=FALSE)
##' ghtml("A node in a Galton-Watson tree has 0 or 2 offspring.<br /> In this case each has equal chance.", cont=g)
##' gseparator(cont=g)
##' tr <- gtree(offspring=offspring, icon.FUN=TRUE, cont=g)
##' size(tr) <- c(300,300)
##' b <- gbutton("Try again", cont=g, handler=function(h,...) tr$update())
##' visible(w) <- TRUE
gtree <- function(offspring = NULL,
                  offspring.data = NULL,
                  icon.FUN = NULL,
                  chosencol = 1,
                  multiple = FALSE, 
                  handler = NULL, action = NULL,
                  container = NULL,
                  ...,
                  width=NULL,
                  height=NULL,
                  ext.args=NULL
                  ) {

  tr <- GTree$new(container$toplevel)
  tr$init(offspring, offspring.data, icon.FUN, chosencol,
          multiple, handler, action, container, ...,
          width=width, height=height, ext.args=ext.args)
  tr
}

##' base class for gtree
##' @nord
GTree <- setRefClass("GTree",
                     contains="ExtWidget",
                     fields=list(
                       proxy="ANY",
                       offspring = "ANY",
                       chosencol="integer",
                       path = "character"
                       ),
                     methods=list(
                       init=function(offspring, offspring.data, icon.FUN, chosencol,
                         multiple, handler, action, container, ...,
                         width=NULL, height=NULL, ext.args=NULL) {


                         constructor <<- "Ext.tree.TreePanel"
                         transport_signal <<- "click"
                         
                         offspring <<- offspring
                         ## create proxy
                         proxy <<- ExtTreeProxy$new(container$toplevel)
                         proxy$init(offspring, offspring.data, icon.FUN, chosencol, multiple)


                         ## arg_list
                         arg_list <- list(
                                          useArrows=TRUE,
                                          autoScroll=TRUE,
                                          animate=TRUE,
                                          border=FALSE,
                                          enableDrag=TRUE,
                                          trackMouseOver=TRUE,
                                          rootVisible=FALSE,
                                          loader = String(sprintf("new Ext.tree.TreeLoader(%s)",
                                            toJSObject(list(
                                                            dataUrl=String("proxy_url"),
                                                            requestMethod="GET",
                                                            baseParams=list(
                                                              id=proxy$get_id(),
                                                              session_id=String("session_id"),
                                                              path=0
                                                            )
                                                            )
                                            ))
                                            ),
                                          root=list( # must have root node
                                            expanded=TRUE,
                                            nodeType='async',
                                            draggable=FALSE,
                                            id= '0'
                                            ),
                                          width=width,
                                          height=height
                                          )
                         add_args(arg_list)

                         if(!is.null(ext.args))
                           add_args(ext.args)

                           container$add_dots(.self, ...)                           

                         write_constructor()
                         add_details(container, handler, action)

                         ## nodeToPath
                         cmd <- paste("nodeToPath = function(n) {",
                                      "var a = new Array();",
                                      "var ids = new Array();",
                                      "var path = new Array();",
                                      "var p = n.parentNode;",
                                      "while (p) {",
                                      "  a.push(p.indexOf(n));",
                                      "  ids.push(n.id);",
                                      "  path.push(n.text);",
                                      "  n = p;",
                                      "  p = n.parentNode",
                                      "};",
                                      "var param={value:{index: a.reverse(), ids: ids.reverse().join(':'), path: path.reverse()}};",
                                      "return param;",
                                      "};",
                                      sep="")
                         add_js_queue(cmd)
                         
                         ## beforeload puts in id and session id
                         cmd <- paste(sprintf("%s.getLoader().on('beforeload', function(loader, node) {", get_id()),
                                      sprintf("loader.baseParams.id = %s;", ourQuote(proxy$get_id())),
                                      "loader.baseParams.session_id = session_id;",
                                      "loader.baseParams.path = nodeToPath(node).value.ids;",
                                      "});",
                                      sep="")
                         add_js_queue(cmd)
                         ## we want to remove items when we collapse, but this doesn't work. Just collapse
                         cmd <- paste(sprintf("%s.on('beforecollapsenode', function(node, deep, anim) {", get_id()),
                                      "node.collapseChildNodes(true);",
                                      "})",
                                      sep="")
                          add_js_queue(cmd)

                       },
                       transport_fun = function() {
                         ## we traverse to find the ids (0-based)
                         "var param = nodeToPath(w);"
                       },
                       process_transport = function(value, ...) {
                         ## transports in a list with id and path
                         value <<- value$index + 1
                         path <<- unlist(value$ids)
                       },
                       update = function(...) {
                         "Update tree"
                         cmd <- paste(sprintf("var rootNode = %s.getRootNode();", get_id()),
                                      sprintf("%s.getLoader().load(rootNode);", get_id()),
                                      "rootNode.expand();",
                                      sep="")
                         add_js_queue(cmd)
                       },
                       get_value = function(index=FALSE, drop=TRUE,...) {
                         "Get selected value, in trees case a path or path indices"
                         if(index) {
                           value
                         } else {
                           if(drop)
                             tail(path, n=1)
                           else
                             path
                         }
                       },
                       set_value = function(value, ...) {
                         "Set value based on path, eg. c(1,2,1). Fires 'click' event to update path, value"
                         cmd <- paste(sprintf("var a = %s;", toJSArray(value - 1)),
                                      "var k = a.length;",
                                      sprintf("var n = %s.getRootNode();", get_id()),
                                      "Ext.each(a.slice(0, a.length-1), function(i) {",
                                      "  n = n.childNodes[i];",
                                      "  n.expand(false)",
                                      "});",
                                      "n = n.childNodes[a[a.length-1]];",
                                      "n.select();",
                                      "n.fireEvent('click',Ext.apply(n, {node:n}));",
                                      sep="")
                         add_js_queue(cmd)
                       },
                       ## handlers
                       add_handler_changed = function(...) {
                         "Main handler is doubleclick. Single click is for selection, double ofr action"
                         add_handler_doubleclick(...)
                       },
                       add_handler_doubleclick = function(...) {
                         add_R_callback("dblclick", ...)
                       }
                       
                       
                       ))
 
                         
