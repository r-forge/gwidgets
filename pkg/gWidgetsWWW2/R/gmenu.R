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

##' Menubar implementation
##'
##' A menubar for gwindow instances
##' %
##' Menu items are specified with a named list. The heirarchical
##' nature of the list maps to the heirarchical menu structure, with
##' the names giving the menu names. The menu actions are specified
##' using \code{gaction} elements. These may also be
##' \code{gseperator()} instances (no parent necessary here).
##' %
##' Menubars should only be added to \code{gwindow} instances, but
##' this is not strictly enforced, any \code{Ext.Panel}-based
##' container would do.
##' @param menulist list of actions. Actions must have parent specified
##' @param popup Logical. ignored for now
##' @param action parameterizes handler in action
##' @param container parent container, a \code{gwindow} instance
##' @param ... passed to add method of parent container
##' @return an ExtWidget object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' stub = function(...) galert("stub", parent=w)
##' l <- list(
##' File = list(open=gaction("Open", handler=stub, parent=w),
##'             new = gaction("New", handler=stub, parent=w),
##'             gseparator(),
##'             quit = gaction("Quit", handler=stub, parent=w)),
##' Edit = list(save = gaction("Save", handler=stub, parent=w))
##' )
##' m <- gmenu(l, cont=w)
##' gtext("Text goes here", cont=w)
gmenu <- function(menulist,  popup = FALSE, action=NULL, container = NULL,...) {
  m <- GMenu$new(container$toplevel)
  m$init(menulist, action, container)
  m
}

##' base class for menu instances.
##' @name gmenu-class
GMenu <- setRefClass("GMenu",
                     contains="ExtWidget",
                     field=list(
                       stub="ANY"
                       ),
                     method=list(
                       init=function(menulist, action=NULL, container) {

                         cmd <- sprintf("var %s = %s.getTopToolbar()",
                                       get_id(), container$get_id())
                         add_js_queue(cmd)

                         Array$new(menulist)$each(function(i, key, value, ...) {
                           make_menu_item(..., index=i, item=value, nm=key)
                         }, get_id())

                         call_Ext("doLayout")
                       },
                       make_menu_item = function(id, index, item, nm) {
                         if(is.list(item)) {
                           ## Make menu with new id
                           id1 <- sprintf("%s_%s",id, index)
                           cmd <- sprintf("var %s = new Ext.menu.Menu();", id1)
                           add_js_queue(cmd)
                           cmd <- sprintf("%s.add({text: %s, menu: %s});", id, ourQuote(nm), id1)
                           add_js_queue(cmd)

                           ## then recurse
                           Array$new(item)$each(function(index, key, value, ...) {
                             make_menu_item(..., index=index, item=value, nm=key)
                           }, id1)
                         } else if(is(item, "GAction")) {
                           cmd <- sprintf("%s.add(%s);", id, item$get_id())
                           add_js_queue(cmd)
                         } else if(is(item, "GSeparator")) {
                           cmd <- sprintf("%s.addSeparator();", id)
                           add_js_queue(cmd)
                         }
                       }
                       ))


