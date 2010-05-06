##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

## gtoolbar, similar to gmenu
## need to incorporate delete/add methods as in imenu


setClass("gToolbarQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## turn a list into a uimgr object
setMethod(".gtoolbar",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   toolbarlist,
                   style = c("both","icons","text","both-horiz"),
                   action=NULL,
                   container=NULL, ...) {

            force(toolkit)

            style = match.arg(style)
            QtToolButtonStyle <-
              c("both"=3L,
                "icons"=0L,
                "text"=1L,
                "both-horiz"=2L)


            
            tb <- Qt$QToolBar()
            tb$setToolButtonStyle(QtToolButtonStyle[style])


            obj = new("gToolbarQt",block=tb, widget=tb,
              toolkit=toolkit, ID=getNewID(),e = new.env())

            tag(obj,"toolbarlist") <- toolbarlist

            if(!is.null(container))
              add(container, obj, ...)

            .mapListToToolBar(tb, toolbarlist)

            invisible(obj)
  
          })


## helpers

.mapListToToolBar = function(tb, lst, style) {
  for(i in names(lst)) {
    tmp <- lst[[i]]
    label <- i

        
    if(.isgSeparatorQt(tmp)) {
      tmp <- Qt$QAction(Qt$QWidget())
      tmp$setSeparator(TRUE)
    }

    if(.isLeaf(tmp) && !.isgActionQt(tmp)) {
      tmp <- .leafToAction(tmp)
    }

    if(.isgActionQt(tmp))
      tb$addAction(getWidget(tmp))
    
  }
}


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToolbarQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "toolbarlist")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gToolbarQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(!is.list(value)) 
                     stop("A toolbar requires a list to define it.")

                   tb = getWidget(obj)
                   tb$clear()
                   .mapListToToolBar(tb, value)                   

                   tag(obj,"toolbarlist") <- value
                   
                   ##  all done
                   return(obj)
                 })

## returns list, or part of list
setMethod("[",
          signature(x="gToolbarQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gToolbarQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = tag(x,"toolbarlist")
            if(missing(i))
              return(lst)
            else
              return(lst[[i]])
          })

setReplaceMethod("[",
                 signature(x="gToolbarQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gToolbarQt"),
          function(x, toolkit, i, j, ..., value) {
            if(!is.list(value))
              stop("assignment must be a list defining a (part) of a toolbar.")
            lst = tag(x,"toolbarlist")
            if(missing(i))
              lst = value
            else
              lst[[i]] = value
            
            svalue(x) <- lst
            
            return(x)
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToolbarQt", value="list"),
          function(obj, toolkit, value,  ...) {
            svalue(obj) <- c(svalue(obj), value)
          })

## (from gmenu)
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt",obj="gToolbarQt"),
          function(obj, toolkit, widget,  ...) {
            ## widget can be gToolBar or a list
            if(is.character(widget)) {
              lst = widget                    # else assume its a character
            } else if(is(widget,"gComponentQt")) {
              lst = svalue(widget)
              lst = names(lst)
            } else if(is.list(widget)) {
              lst = names(widget)
            } else {
              warning("Must be either a vector of names, a list, or a gToolbar instance")
              return()
            }
            
            cur.list = svalue(obj)             
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update toolbar
            svalue(obj) <- cur.list
          })

### no method to set style, use tag(obj,"style")<-"theStyle" instead
