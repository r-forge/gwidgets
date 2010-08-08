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

## XXX To do a popup menu for e=Qt$QLineEdit, say:
## e$setContextMenuPolicy(2L)
## a = Qt$QAction("label", Qt$QFrame())
## e$addAction(a)
## connect:
## qconnect(e, "textChanged, function(text) a$setText(text))
## that make a depend on the context.


setClass("gMenuQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )
setClass("gMenuItemQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )
setClass("gMenuPopupQt",
         contains="gMenuQt",
         prototype=prototype(new("gComponentQt"))
         )



## menulist is a list of lists with named components. Each named sub
## is a submenu.  a leaf consistis of handler= (required), lab

## put menu in group,
## a menubar is a map from a list into a menubar
## constructor
setMethod(".gmenu",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   menulist, 
                   popup = FALSE,
                   action = NULL,
                   container=NULL, ...) {
            
            force(toolkit)

            
            ##
            if(popup) {
              topMenu <- Qt$QMenu()
              obj = new("gMenuPopupQt", block=topMenu, widget=topMenu,
                toolkit=toolkit, ID=getNewID(), e = new.env())
            } else {
              topMenu <- Qt$QMenuBar()
              obj = new("gMenuQt", block=topMenu, widget=topMenu,
                toolkit=toolkit, ID=getNewID(), e = new.env())
            }
            
            mapListToMenuBar(menulist, topMenu)
            tag(obj, "menulist") <- menulist

            if(!is.null(container) && !popup)
              add(container, obj,...)

            invisible(obj)
          })



### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gMenuQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            tag(obj, "menulist")
          })

## three cases for value: list, gMenuQt, guiWidget push down
## make a menubar, then replace current -- isn't working for popup case
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gMenuQt",
                           value="list"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   menulist = value            # value is a list
                   if(!is.list(menulist))
                     stop("value is not a menubar or a list")
                   
                   
                   mb = getWidget(obj)
                   mb$clear()
                   mapListToMenuBar(menulist, mb)

                   ## store for later?
                   tag(obj,"menulist") <- menulist

                   return(obj)
                 })

## get list, and then call previous
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gMenuQt",
                           value="gMenuQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit, index, ...) <- svalue(value)
                   return(obj)
                 })

## call previous after getting list
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gMenuQt",
                           value="guiWidget"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .svalue(obj,toolkit,index, ...) <- svalue(value@widget)
                   return(obj)
                 })

## this is for adding a menu to a menu
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt", value="guiWidget"),
          function(obj, toolkit,  value, ...) {
            .add(obj, toolkit, value@widget)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt", value="gMenuQt"),
          function(obj, toolkit,  value, ...) {
            orig.list = svalue(obj)
            add.list = svalue(value)
            new.list = c(orig.list, add.list)
            ## redoes menu XXX COuld make it just add if speed is an issue
            svalue(obj) <- new.list
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="gMenuQt", value="list"),
          function(obj, toolkit,  value, ...) {
            mb = getWidget(obj)
            mapListToMenuBar(value, mb)
          })

## Add in gWindow.
## SHould I have an add for a container?
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",
                    obj="gContainerQt", value="gMenuQt"),
          function(obj, toolkit,  value, ...) {
            XXX("Adding to a container isn't suggested, try a gwindow() instance")
            callNextMethod()
          })


#####
## Add a popup menu
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="guiWidget", value="gMenuPopupQt"),
          function(obj, toolkit,  value, ...) {
            .add(obj@widget, toolkit, value@widget)
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gWidgetQt", value="gMenuPopupQt"),
          function(obj, toolkit,  value, ...) {
            w <- getWidget(obj)
           if(!is.null(w$setContextMenuPolicy))
             w$setContextMenuPolicy(Qt$Qt$ActionsContextMenu) # override default Qt::ContextMenuPolicy
           else
             return()                   # cant do popup

           lst <- svalue(value)
           for(i in lst) {
             act <- NULL
             if(.isgSeparatorQt(i)) {
               act <- Qt$QAction(Qt$QWidget())
               act$setSeparator(TRUE)
             } else if(.isgActionQt(i)) {
               act <- getWidget(i)
             } else if(is.list(i)) {
               add(obj, i)
             }
             if(!is.null(act))
               w$addAction(act)
           }
         })




## "widget" is either a gMenu, list or just names to delete
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt",
                    widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget,...)
          })

setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt",
                    widget="gWidgetQt"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt",
                    widget="gMenuQt"),
          function(obj, toolkit, widget, ...) {
            .delete(obj,toolkit,svalue(widget), ...)
          })


setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitQt", obj="gMenuQt",
                    widget="list"),
          function(obj, toolkit, widget, ...) {
            lst = widget                    # else assume its a character
            
            cur.list = svalue(obj)
            for(i in lst) {
              ## we delete *last* entry with this name, hence this awkwardness
              theNames = names(cur.list)
              if(i %in% theNames) {
                j = max(which(i == theNames))
                if(!is.null(cur.list[[j]])) cur.list[[j]] <- NULL
              }
            }
            ## now update menubar
            svalue(obj) <- cur.list
          })

## give vector notation
setMethod("[",
          signature(x="gMenuQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gMenuQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            lst = svalue(x)
            if(missing(i))
              return(lst)
            else
              return(lst[i])
          })

setReplaceMethod("[",
                 signature(x="gMenuQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gMenuQt"),
          function(x, toolkit, i, j, ..., value) {
            lst = svalue(obj)
            theNames = names(lst)
            if(is.character(i))
              i = max(which(i %in% theNames))
            lst[[i]] <- value[[1]]
            theNames[i] = names(value)
            names(lst) = theNames
            svalue(obj) <- lst
            return(obj)
          })

##################################################
## helper functions

makeSubMenu = function(lst, label, parentMenu) {
  subMenu <- parentMenu$addMenu(label)
  
  sapply(names(lst),function(i) {
    
    tmp <- lst[[i]]
    if(is.list(tmp) && !is.null(tmp$label))
      label <- tmp$label
    else
      i

    
    
    
    if(.isgSeparatorQt(tmp)) {
      tmp <- Qt$QAction(Qt$QWidget())
      tmp$setSeparator(TRUE)
    }
    
    if(.isLeaf(tmp) && !.isgActionQt(tmp)) {
      tmp <- .leafToAction(tmp, label)
    }
    
    if(.isgActionQt(tmp)) {
      tmp <- getWidget(tmp)
      subMenu$addAction(tmp)
    } else {
      makeSubMenu(tmp, tmp$label, subMenu)
    }
    
  })
}


mapListToMenuBar <- function(menulist, topMenu) {
  if(!.isLeaf(menulist[[1]])) {
    sapply(names(menulist), function(i) 
           makeSubMenu(menulist[[i]],label=i,topMenu))
  } else {
    ## toplevel
    sapply(names(menulist), function(i) {
      label <- getWithDefault(menulist$label, i)
      tmp <- menulist[[i]]
      if(.isgSeparatorQt(tmp)) {
        tmp <- Qt$QAction(Qt$QWidget())
        tmp$setSeparator(TRUE)
      }
      if(!.isgActionQt(tmp)) {
        tmp <- .leafToAction(tmp, label)
      }
      tmp <- getWidget(tmp)
      topMenu$addAction(tmp)
    })
  }
}



## some helper functions for this and gtoolbar
.isLeaf <- function(lst) {
  .isgActionQt(lst) ||
  (is.list(lst) & (!is.null(lst$handler) | !is.null(lst$separator)))
}

.leafToAction <- function(tmp, label=tmp$label) {
  .gaction(guiToolkit("Qt"),
          label=getWithDefault(label,"label"),
          tooltip=tmp$tooltip,
          icon=tmp$icon,
          handler=tmp$handler,
          action=tmp$action,
          key.accel=tmp$key.accel)
}

.isgSeparatorQt <- function(tmp) {
  (is.list(tmp) && !is.null(tmp$separator)) ||
  (is(tmp,"guiWidget") && is(tmp@widget, "gSeparatorQt")) ||
  is(tmp, "gSeparatorQt")
}

.isgActionQt <- function(obj) {
  (is(obj,"guiWidget") && is(obj@widget, "gActionQt")) ||
  is(obj,"gActionQt") ||
  is(obj, "QAction")
}

    
