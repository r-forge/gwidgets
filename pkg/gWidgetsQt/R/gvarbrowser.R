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

## use QTreeWidget to make workspace browser
## Can be on the slow side, so we add option to auto refresh



##' a base tree widget item
##' This is here so we can store a digest value for comparison
qsetClass("OurTreeWidgetItem", Qt$QTreeWidgetItem, function() {
  this$digest <- NULL
})
qsetMethod("digest", OurTreeWidgetItem, function() digest)
qsetMethod("setDigest", OurTreeWidgetItem, function(value) {
  this$digest <- value
})

##' A base tree widget which allows drag and drop
qsetClass("OurTreeWidget", Qt$QTreeWidget, function(parent=NULL) {
  super(parent)
  this$dndStartPosition <- NULL
})

##' drag and drop
qsetMethod("prepareDrag", OurTreeWidget, function(e) {
  item <- itemAt(e$pos())
  
  getVal <- function(item)  path <- item$data(0, role=0)
  path <- getVal(item)
  while(!is.null(item <- item$parent())) {
    path <- c(getVal(item), path)
  }

  obj <- get(path[1], envir=.GlobalEnv)
  if(length(path) > 1)
    obj <- obj[[path[-1]]]

  ## oh what to return
  val <- list(dropdata=paste(path, collapse="$"), obj=obj)
  # val <- varname=paste(path, collapse="$")

  
  md <- Qt$QMimeData()
  md$setData("R/serialized-data", serialize(val, NULL))

  drag <- Qt$QDrag(this)
  drag$setMimeData(md)
  
  drag$exec()
})

qsetMethod("mousePressEvent", OurTreeWidget, function(e) {
  if (e$buttons() == Qt$Qt$LeftButton)
     this$dndStartPosition <- e$pos();
   super("mousePressEvent", e)
})

qsetMethod("mouseMoveEvent", OurTreeWidget, function(e) {
  if ((e$buttons() & Qt$Qt$LeftButton) && !is.null(dndStartPosition)) {
    dist <- (e$x() - dndStartPosition$x())^2 +  (e$y() - dndStartPosition$y())^2
    if (dist >= Qt$QApplication$startDragDistance()^2)
      this$prepareDrag(e)
  }
  super("mouseMoveEvent",e)
})

  

##' Main wsbrowser class. Its a widget, but mostly centered around the tree
qsetClass("WSBrowser", Qt$QWidget, function(parent=NULL) {
  super(parent)

  ## filter by class
  this$classFilter <- Qt$QComboBox()

  knownTypes <- getWithDefault(getOption("gvarbrowserKnownTypes"))
  if(is.null(knownTypes))
     knownTypes <- getOption("knownTypes")
  if(is.null(knownTypes))
    knownTypes <-  gWidgets:::knownTypes
  this$knownTypes <- knownTypes
  
  classFilter$addItems(names(knownTypes))
  qconnect(classFilter, "activated", function(...) {
    updateTopLevelItems()
  })

  ## tree widget
  this$tr <- OurTreeWidget()
  tr$setColumnCount(2)                    # name, class
  tr$setHeaderLabels(gettext(c("Name", "Class")))

  ## update button
  this$ub <- Qt$QPushButton(gettext("Refresh"))
  qconnect(ub, "clicked", function() this$updateTopLevelItems())
  ub$setSizePolicy(Qt$QSizePolicy$fixed, Qt$QSizePolicy$fixed)

  ## auto update checkbox
  this$cb <- Qt$QCheckBox(gettext("Auto refresh"))

  lyt <- Qt$QGridLayout()
  setLayout(lyt)

  lyt$addWidget(Qt$QLabel("Filter by:"), 0, 0, 1, 1, Qt$Qt$AlignRight | Qt$Qt$AlignTop)
  lyt$addWidget(classFilter, 0, 1, 1, 1)
  lyt$addWidget(tr, 1, 0, 1, 2)
  lyt$addWidget(ub, 2, 0, 1, 1, Qt$Qt$AlignLeft)
  lyt$addWidget(cb, 2, 1, 1, 1, Qt$Qt$AlignRight)

  lyt$setColumnStretch(0, 1)
  lyt$setRowStretch(1, 1)

  updateTopLevelItems()
  
  ## setup timer to update if checked
  this$timer <- Qt$QTimer(this)
  qconnect(timer, "timeout", function() {
    if(cb$checkState() & Qt$Qt$Checked) {
      updateTopLevelItems()
    }
  })
  startTimer()
})

## get the tree widget
qsetMethod("treeWidget", WSBrowser, function() tr)

##' method call to stop the timer
qsetMethod("stopTimer", WSBrowser, function() {
  timer$stop()
})

##' method call to start the times
##' @param int is interval in milliseconds
qsetMethod("startTimer", WSBrowser, function(int=2000L) {
  timer$start(int)
})

##' Make a new item. Sets icons and children into top-level item
##' Why doesn't this work as a method?
##' We have issues with recursion within a method. So we make this a function
##'
##' @param varname variable name as character
##' @param obj actual object, if not passed in found in global environment
##' @param toplevel are we making a top-level item (if so, we take a digest to compare)
..makeItem <- function(varname, obj=NULL, toplevel=FALSE) {
  if(is.null(obj))
    obj <- get(varname, envir=.GlobalEnv)

  item <- OurTreeWidgetItem()
  if(toplevel)
    item$setDigest(digest(obj))
  item$setText(0, varname)
  item$setIcon(0, findStockIcon(obj))
  item$setText(1, paste(class(obj), collapse=", "))
  
  if(is.recursive(obj) && !is.null(attr(obj, "names"))) {
    item$setChildIndicatorPolicy(Qt$QTreeWidgetItem$ShowIndicator)
    for(i in names(obj)) {
      if(i != "") {
        newItem <- ..makeItem(i, obj[[i]], toplevel=FALSE)
        item$addChild(newItem)
      }
    }
  }
  item
}


##' update the top-level items
##' @param classes vector of class names to narrow displayed values (also speeds things up)
##' This is passed in or set by the classFilter
qsetMethod("updateTopLevelItems", WSBrowser,  function(classes=NULL) {

  ## get classes from filter. XXX
  if(is.null(classes)) {
    if((i <- classFilter$currentIndex) >= 0) {
      classes <- knownTypes[[i+1]]      # editable?
    }
  }

  
  addItem <- function(tr, varname) {
    newItem <- ..makeItem(varname, obj=NULL, toplevel=TRUE)
    tr$addTopLevelItem(newItem)
  }
  
  removeItem <- function(tr, item) {
    root <- tr$invisibleRootItem()
    root$removeChild(item)
  }
  
  replaceItem <- function(tr, item, varname) {
    removeItem(tr, item)
    addItem(tr, varname)
  }

  
  ## objects in workspace
  objs <- ls(envir=.GlobalEnv)
  ## trim down here -- before tree level
  if(!is.null(classes)) {
    ind <- sapply(objs, function(i) {
      x <- get(i, envir=.GlobalEnv)
      for(i in classes)
        if(is(x,i))
          return(TRUE)
      return(FALSE)
    })
    objs <- objs[ind]
  }
  
  ## current objects in widget
  curObjs <- lapply(seq_len(tr$topLevelItemCount), function(i) {
    item <- tr$topLevelItem(i-1)
    list(item=item, value=item$data(0, role=0) )               # better role?
  })
  cur <- sapply(curObjs, function(i) i$value)
  names(curObjs) <- cur
  
  ## we have three types here:
  removeThese <- setdiff(cur, objs)
  maybeSame <- intersect(cur, objs)
  addThese <- setdiff(objs, cur)

  tr$setUpdatesEnabled(FALSE)
  for(i in removeThese) {
    item <- curObjs[[i]]$item
    removeItem(tr, item)
  }

  
  for(i in maybeSame) {
    obj <- get(i, envir=.GlobalEnv)
    if(digest(obj) != curObjs[[i]]$item$digest()) {
      replaceItem(tr, curObjs[[i]]$item, i)
    }
  }

  for(i in addThese) 
    addItem(tr, i)

  tr$sortItems(0, Qt$Qt$AscendingOrder)
  tr$setUpdatesEnabled(TRUE)  
})


## traverse
##' find an index (1-based) from an item
##' @param item an item
##' @note replaces a broken feature in the API
qsetMethod("indexFromItem", WSBrowser, function(item) {
  parent <- item$parent()
  if(is.null(parent))
    parent <- tr$invisibleRootItem()
  parent$indexOfChild(item) + 1
})

##' find the path from the item
##' @param item an item
##' @param index if TRUE return numeric index vector, else a character vector of first-column keys
qsetMethod("pathFromItem", WSBrowser, function(item, index=TRUE) {
  getVal <- function(item) {
    if(index) 
      path <- tr$indexFromItem(item)
    else
      path <- item$data(0, role=0)
    path
  }
  path <- getVal(item)
  while(!is.null(item <- item$parent())) {
    path <- c(getVal(item), path)
  }
  path
})


##' From a path (1-based numeric specification) return an item
##' If path is not valid, returns NULL
##' @param path 1-based value, e.g. c(1,2,3) is third child of second child of first child
qsetMethod("itemFromPath", WSBrowser, function(path) {
  item <- tr$invisibleRootItem()
  for(i in path) {
    item <- item$child(i)
    if(is.null(item))
      return(NULL)
  }
  item
})



##' The main class
setClass("gVarbrowserQt",
         representation(filter="guiComponent"),
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

##' toolkit constructor for gvarbrowser
setMethod(".gvarbrowser",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   handler = NULL,
                   action = "summary",
                   container = NULL,
                   ...) {

            force(toolkit)
            

            ## fix handler if action is non-null
            if(is.null(handler) && !is.null(action)) {
              handler = function(h, ...) {
                value <- svalue(h$obj)
                if(nchar(value)) {
                  if (!is.null(action))
                    print(do.call(h$action, list(svalue(value))))
                }
              }
            }


            w <- WSBrowser()
            
            obj <- new("gVarbrowserQt", block=w, widget=w$treeWidget(),
              toolkit=toolkit, e=new.env(), ID=getNewID())

            if(!is.null(handler)) {
              id <- addhandlerdoubleclick(obj, handler=handler, action=action)
              tag(obj, "handler.id") <- id
            }


            if(!is.null(container))
              add(container, obj, ...)
            
            ## all done
            return(obj)
          })

### methods
## push methods and handlers down to tree in this case

##' svalue returns the variable name. If a child, puts "$" between the parts.
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gVarbrowserQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            values <- obj[]
            value <- paste(values, collapse = "$")
            return(value)
          })

##' returns the path (keyword path) of the object
setMethod("[",
          signature(x="gVarbrowserQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x,guiToolkit("Qt"), i, j, ..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gVarbrowserQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            widget <- getBlock(x)
            tr <- getWidget(x)
            item <- tr$currentItem()
            if(is.null(item)) 
              return(character(0))
            
            path <- widget$pathFromItem(item, index=FALSE)
            if(missing(i))
              path
            else
              path[i]
          })

## handlers

##' Change handler is for double click
setMethod(".addhandlerchanged",  signature(toolkit="guiWidgetsToolkitQt",obj="gVarbrowserQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerdoubleclick(obj, toolkit, handler, action, ...)
          })

##' single click handler, activated also does keyboard input
setMethod(".addhandlerclicked",  signature(toolkit="guiWidgetsToolkitQt",obj="gVarbrowserQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(item, column, h, ...) handler(h, item, column, ...)
            addhandler(obj, "itemActivated", f, action, ...)
          })

##' Double click handler
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gVarbrowserQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f <- function(item, column, h, ...) handler(h, item, column, ...)
            addhandler(obj, "itemDoubleClicked", f, action, ...)
          })


