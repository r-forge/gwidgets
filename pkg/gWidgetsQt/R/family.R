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


## widget heirarchy

## setParent
setGeneric("setParent", function(obj, parent) standardGeneric("setParent"))
setMethod("setParent", signature(obj="guiWidget",parent="gContainerQt"),
          function(obj, parent) setParent(obj@widget, parent))
setMethod("setParent", signature(obj="gWidgetQt",parent="guiWidget"),
          function(obj, parent) setParent(obj, parent@widget))
setMethod("setParent", signature(obj="gWidgetQt",parent="gContainerQt"),
          function(obj, parent) tag(obj, "parent") <- parent)


setGeneric("getParent", function(obj) standardGeneric("getParent"))
setMethod("getParent", signature(obj="guiComponent"),
          function(obj) {
            getParent(obj@widget)
          })
setMethod("getParent", signature(obj="gWidgetQt"),
          function(obj) tag(obj, "parent"))


setGeneric("removeParent", function(obj) standardGeneric("removeParent"))
setMethod("removeParent", signature(obj="guiComponent"),
          function(obj) removeParent(obj@widget))
setMethod("removeParent", signature(obj="gWidgetQt"),
          function(obj) tag(obj, "parent") <- NULL)


## getTopLevel
setGeneric("getTopLevel", function(obj) standardGeneric("getTopLevel"))
setMethod("getTopLevel", signature(obj="guiContainer"),
          function(obj) {
            getTopLevel(obj@widget)
          })
setMethod("getTopLevel", signature(obj="guiComponent"),
          function(obj) {
            getTopLevel(obj@widget)
          })
setMethod("getTopLevel", signature(obj="gWidgetQt"),
          function(obj) {
            ## recurse up, until null
            parent <- tag(obj, "parent")
            while(!is.null(parent)) {
              obj <- parent
              parent <- tag(obj, "parent")
            }
            return(obj)
          })
## addChild
setGeneric("addChild", function(obj, child) standardGeneric("addChild"))
setMethod("addChild", signature(obj="guiWidget", child="guiWidget"),
          function(obj, child) {
            addChild(obj@widget, child)
          })
setMethod("addChild", signature(obj="guiWidget", child="gWidgetQt"),
          function(obj, child) {
            addChild(obj@widget, child)
          })
setMethod("addChild", signature(obj="gContainerQt", child="guiWidget"),
          function(obj, child) {
            addChild(obj, child@widget)
          })
setMethod("addChild", signature(obj="gContainerQt", child="gWidgetQt"),
          function(obj, child) {
            l <- tag(obj, "children")
            if(is.null(l))
              l <- list()
            l <- c(l, child)
            tag(obj, "children") <- l
          })
          
## removeChild
setGeneric("removeChild", function(obj, child) standardGeneric("removeChild"))
setMethod("removeChild", signature(obj="guiComponent", child="gWidgetQt"),
          function(obj, child) {
            removeChild(obj@widget, child)
          })
setMethod("removeChild", signature(obj="gContainerQt", child="guiComponent"),
          function(obj, child) {
            removeChild(obj, child@widget)
          })
setMethod("removeChild", signature(obj="gContainerQt", child="gWidgetQt"),
          function(obj, child) {
            l <- tag(obj, "children")
            ind <- sapply(l, function(i) digest(i) != digest(child))
            l <- l[ind]
            tag(obj, "children") <- l
          })
          
## getChildren
setGeneric("getChildren", function(obj) standardGeneric("getChildren"))
setMethod("getChildren", signature(obj="guiComponent"),
          function(obj) {
            getChildren(obj@widget)
          })
setMethod("getChildren", signature(obj="gWidgetQt"),
          function(obj) {
            tag(obj, "children")
          })
          

## noChildren
setGeneric("noChildren", function(obj) standardGeneric("noChildren"))
setMethod("noChildren", signature(obj="guiComponent"),
          function(obj) noChildren(obj@widget))
setMethod("noChildren", signature(obj="gWidgetQt"),
          function(obj) length(getChildren(obj)))

  
## findChildFromQWidget
## issue with inheritance, make this a function call, not a method
findChildFromQWidget <- function(obj, widget) {
  l <- getChildren(obj)
  for(i in l) {
    if(digest(getWidget(i)) == digest(widget))
      return(i)
  }
  return(NA)
}

## setGeneric("findChildFromQWidget", function(obj, widget) standardGeneric("findChildFromQWidget"))
## setMethod("findChildFromQWidget", signature(obj="gContainerQt", widget="QWidget"),
##           function(obj, widget) {
##             l <- getChildren(obj)
##             for(i in l) {
##               if(digest(getWidget(i)) == digest(widget))
##                 return(i)
##             }
##             return(NA)
##           })




