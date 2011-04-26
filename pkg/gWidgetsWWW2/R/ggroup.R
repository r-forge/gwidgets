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


##'  Group, or box, container
##'
##' A box container. Can pack in left to right or top to bottom.
##' The expand, fill, anchor arguments are not (yet?) implemented
##' @param horizontal left or right, or top to bottom (FALSE)
##' @param spacing body spacing
##' @param use.scrollwindow ignored
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param width optional width in pixels
##' @param height optional height in pixels
##' @param ext.args extra arguments to pass to Ext constructor
##' @return an ExtContainer object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE) ## top to bottom
##' for(i in 1:10) gbutton(i, cont=g)
##' ## add/delete
##' ctr <- 1 ### for label
##' addRow <- function(g) {
##' g1 <- ggroup(cont=g)
##' gbutton("x", cont=g1, handler=function(h,...) {delete(g, g1)})
##' glabel(paste("Click x to delete", ctr), cont=g1)
##' ctr <<- ctr + 1
##' }
##' g1 <- gframe("add/delete", cont=g, horizontal=FALSE)
##' addRow(g1)
##' f <- gframe("Adding and deleting", cont=g, horizontal=FALSE)
##' gbutton("+", cont=f, handler=function(h,...) addRow(f))
ggroup <- function(
                   horizontal=TRUE,
                   spacing=5,
                   use.scrollwindow = FALSE,
                   container,
                   ...,
                   width=NULL,
                   height=NULL,
                   ext.args = NULL
                   ){
  g <- GGroup$new(toplevel=container$toplevel)
  g$init(horizontal, spacing, use.scrollwindow, container, ..., width=width, height=height, ext.args=ext.args)
  g
}


##' base class for ggroup
##' @name ggroup-class
GGroup <- setRefClass("GGroup",
                       contains="ExtContainer",
                       fields=list(
                         stub = "ANY"
                         ),
                       methods=list(
                         init = function(
                           horizontal=TRUE,
                           spacing=5,
                           use.scrollwindow = FALSE,
                           container,
                           ...,
                           width=NULL,
                           height=NULL,
                           ext.args = NULL
                           ) {

                           constructor <<- "Ext.Panel"
                           spacing <<- spacing
                           
                           arg_list <- list(border = TRUE,
                                            ## padding=spacing, # just, margin see add_dots now
                                            hideBorders = FALSE,
                                            autoScroll = as.logical(use.scrollwindow),
                                            width=width,
                                            height=height
                                            )
                           if(horizontal)
                             arg_list[['layout']] <- "hbox"
                           else
                             arg_list[['layoutConfig']] <- list(type="vbox", pack="start")
                           

                           add_args(arg_list)
                           
                           if(!is.null(ext.args))
                             args$extend(ext.args)
                           
                           container$add_dots(.self, ...)                           

                           write_constructor()

                           container$add(.self, ...)
                           
                         }
                           
                         )
                       )
