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

setClass("gTreeQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

## map a list to a tree
setMethod(".gtree",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   offspring = NULL,
                   hasOffspring = NULL,                 # for defining offspring. FUN
                                        # of children only.
                   offspring.data = NULL,
                   col.types = NULL, # data frame with logical
                   icon.FUN = NULL,                      # return stock name --called
                                        # on offspring, returns a
                                        # vector of length nrow
                   chosencol = 1,
                   multiple = FALSE,
                   handler = NULL,
                   action=NULL,
                   container=NULL,
                   ...
                   ) {
            
            force(toolkit)

            ## allow multiple if asked
            if(as.logical(multiple))
              XXX("Set selection mod")
            
            ## warn if chosencol not = 1
            if(chosencol != 1) {
              XXX(gettext("Chosencol is always the first column\n"))
            }
            
            tr <- Qt$QTreeWidget()
            
            obj <- new("gTreeQt", block=tr, widget=tr,
                       ID=getNewID(),
                       e = new.env(),
                       toolkit=toolkit)

            tag(obj,"offspring") <- offspring
            tag(obj,"offspring.data") <- offspring.data
            tag(obj,"icon.FUN")  <-  icon.FUN
            tag(obj,"chosencol") <- chosencol
            tag(obj,"multiple") <- multiple
            ## we need this one to keep track of the expand icons
            tag(obj, "itemsWithChildren") <- list()
            
            ## add children
            update(obj)
            l <- .gtree.makeDfFromPath(obj, c())
            names(obj) <- names(l$os)   # set names

            ## dynamic aspect of tree is here
            qconnect(tr, "itemExpanded", function(item, obj) {
              tr <- getWidget(obj)
              path <- .gtree.getItemPath(tr, item)$path
              children <- .gtree.findChildrenFromPath(obj, path)
              .gtree.addChildItems(obj, item, children)
            }, user.data=obj)

            ## and here
            qconnect(tr, "itemCollapsed", function(item, obj) {
              .gtree.clearChildItems(obj, item)
            }, user.data=obj)


            ## handler
            if(!is.null(handler)) {
              id <- .addhandlerdoubleclick(obj,toolkit,handler,action)
              tag(obj, "handler.id") <- id
            }
            
            ## attach to container
            if(!is.null(container))
              add(container, obj,...)
            
            return(obj)
          })


## we need to keep track of which items have children,
## as there is no way within Qt to do so except childCount
## and we don't have the count until we expand
.gtree.itemHasChild <- function(obj, item) {
  l <- tag(obj, "itemsWithChildren")
  l[[digest(item)]] <- TRUE
  tag(obj, "itemsWithChildren") <- l
}
.gtree.itemHasNoChild <- function(obj, item) {
  l <- tag(obj, "itemsWithChildren")
  l[[digest(item)]] <- NULL
  tag(obj, "itemsWithChildren") <- l
}

.gtree.doesItemHaveChildren <- function(obj, item) {
  l <- tag(obj, "itemsWithChildren")
  id <- digest(item)
  if(is.null(l[[id]]))
    FALSE
  else
    TRUE
}




## helper to make a child item
.gtree.makeChildItem <- function(data, hasChild=FALSE, icon=NULL) {
  ##
  if(length(data) == 0) {
    cat("Need some data")
    data=list("nothing here")
  }
  if(!is.list(data))
    data <- lapply(data, function(i) i)

  if(icon == "")
    icon <- NULL
  item <- list(icon=icon,
               data=data,
               children=list()
               )

  
  if(is.na(hasChild) || !hasChild)
    item$children <- NULL
  
  class(item) <- c("childItem", class(item))
  item
}

.gtree.makeDfFromPath <- function(obj, path) {
  ## first make data frame with
  ## hasChildren, icons, data columns
  offspring <- tag(obj, "offspring")
  offspring.data <- tag(obj, "offspring.data")
  
  os <- offspring(path, offspring.data)
  if(is.null(os) || nrow(os) == 0) {
    ## no children to add
    return(list())
  }
  
  ## icons
  icon.FUN <- tag(obj, "icon.FUN")
  if(is.null(icon.FUN))
    icons <- rep(NULL, nrow(os))
  else
    icons <- icon.FUN(os)
  icons <- sapply(icons, function(i) getWithDefault(i, ""))

  ## do we have to offspring
  hasOffspring <- tag(obj, "hasOffspring")
  if(!is.null(hasOffspring)) {
    offspring <- hasOffspring(os)
  } else if(is.logical(os[,2])) {
    offspring <- os[,2]
    os[,2] <- NULL
  } else {
    offspring <- rep(NULL, nrow(os))
  }

  ## kill factors, make all character
  for(i in 1:ncol(os))
    os[,i] <- as.character(os[,i])

  out <- list(os=os, offspring=offspring, icons=icons)
}

## Make children -- this is a list of childItems
## we need to massage the data frame to do this
.gtree.findChildrenFromPath <- function(obj, path, do.headers=FALSE) {
  l <- .gtree.makeDfFromPath(obj, path)
  os <- l$os

  if(is.null(os) || nrow(os) == 0)
    return(list())
  
  children <- lapply(1:nrow(os), function(i) {
    .gtree.makeChildItem(data=os[i,], hasChild=l$offspring[i],
                        icon=l$icons[i])
                      })
                     
  children       
}

## Method to add children. Children specified as childItems
.gtree.addChildItems <- function(obj, item, children) {
  if(length(children)) {
    for(i in 1:length(children)) {
      child <- children[[i]]
      citem <- Qt$QTreeWidgetItem(child$data[[1]])
      for(j in 1:length(child$data))
        citem$setText(j-1, child$data[[j]])
      if(!is.null(child$icon)) {
        icon <- getStockIconFromName(child$icon)
        if(!is.null(icon))
          citem$setIcon(0, icon)
      }
      if(!is.null(child$children)) {
        citem$setChildIndicatorPolicy(0L) # 0L -- give expand icon
        .gtree.itemHasChild(obj, citem)
      }
      item$addChild(citem)
    }
  }
}


## method to clear
.gtree.clearChildItems <- function(obj, item) {
  tr <- getWidget(obj)
  noChildren <- item$childCount()
  sapply(rev(1:noChildren), function (i) {
    child <- item$takeChild(i - 1)
    item$removeChild(child)
  })
  .gtree.itemHasNoChild(obj, item)
}

## how to find selected index
.gtree.getSelected <- function(tr) {
  ## return selected items.
  items <- tr$selectedItems()
  items
}

## method to find path. Need to pass in tr, item
.gtree.getItemPath <- function(tr, item) {
  path <- c()
  indices <- c()
  ## walk backward to beginning
  parent <- item$parent()
  while(!is.null(parent)) {
    path <- c(item$text(0), path)
    indices <- c(parent$indexOfChild(item) + 1, indices)
    item <- parent
    parent <- item$parent()
  }
  root <- tr$invisibleRootItem()
  path <- c(item$text(0), path)
  indices <- c(root$indexOfChild(item) + 1, indices)
  
  return(list(path=path, indices=indices))
}



## has different arguments, but we mask this with ...
## this has offspringdata as first argument
## update makes root tree elements
setMethod(".update",
          signature(toolkit="guiWidgetsToolkitQt",object="gTreeQt"),
          function(object, toolkit,...) {
            obj <- object     # rename, object from update generic
            tr <- getWidget(obj)
            root <- tr$invisibleRootItem()
            
            theArgs <- list(...)

            tag(obj, "offspring.data") <- getWithDefault(theArgs$offspring.data,
                                                         tag(obj,"offspring.data"))



            ## algorithm
            ## Loop over values already in tree.
            ## If not in children delete from tree
            ## if in children delete from children
            ## if any new ones left add at end
            children <- .gtree.findChildrenFromPath(obj, c(), do.headers=TRUE)
            newChildren <- sapply(children, function(i) i$data[[1]])
            noLongerNeeded <- list()       # from current items
            alreadyThere <- c()             # no need to add
            if(root$childCount() > 0) {
              ## loop over children already there
              for(i in 1:root$childCount()) {
                item <- root$child(i-1)
                curChild <- item$text(0)
                if(!curChild %in% newChildren ) {
                  ## delete from tree
                  noLongerNeeded <- c(noLongerNeeded, item)
                } else {
                  ## delete row from children
                  ## here we grab index
                  ind <- which(curChild == newChildren)
                  alreadyThere <- c(alreadyThere, ind)
                }
              }
            }

            ## delete objects from tree that aren't there any more
            if(length(noLongerNeeded))
              sapply(noLongerNeeded, function(i) root$removeChild(i))

            ## these children are already present
            if(length(alreadyThere)) {
              children[alreadyThere] <- NULL
            }
            
            ## now if any children left, we add
            if(length(children) > 0) {
                nc <- length(children[[1]]$data)
                tr$setColumnCount(nc)
                ## add children
                .gtree.addChildItems(obj, root, children)
            }

            return(NULL)
          })

## use index for the column to override the column returned
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTreeQt"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {

            tr <- getWidget(obj)
            selItems <- tr$selectedItems()

            if(length(selItems) == 0)
              return(c())

            l <- lapply(selItems, function(item) .gtree.getItemPath(tr, item))
            paths <- lapply(l, function(i) i$path)
            indices <- lapply(l, function(i) i$indices)
            
            ## return values
            drop <- getWithDefault(drop, TRUE)
            index <- getWithDefault(index, FALSE)

            if(drop) {
              if(index) {
                vals <- lapply(indices, function(i) tail(i, n=1))
              } else {
                vals <- lapply(paths, function(i) tail(i, n=1))
              }
            } else {
              if(index)
                vals <- indices
              else
                vals <- paths
            }
            if(length(vals) == 1)
              vals <- vals[[1]]

            
            return(vals)
          })

setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTreeQt"),
          function(obj, toolkit, index=NULL, ..., value) {
            index <- getWithDefault(index, TRUE)
            if(!index || !is.numeric(value))
              return(obj)                  # need indices

            tr <- getWidget(obj)
            parent <- tr$invisibleRootItem()
            ## Must check if has child without looking at childCount()
            n <- length(value)
            for(i in seq_along(value)) {
              if(i < n && !.gtree.doesItemHaveChildren(obj, parent)) {
                XXX(gettext("Invalid indices"))
                return(obj)
              }

              if(.gtree.doesItemHaveChildren(obj, parent))
                parent$setExpanded(TRUE)

              if(value[i] >= 1 && value[i] <= parent$childCount()) {
                parent <- parent$child(value[i]-1)
              } else {
                XXX(gettext("Invalid indices"))
                return(obj)
              }
            }
            ## clear selection if not multiple
            if(!tag(obj, "multiple")) {
              selModel <- tr$selectionModel()
              selModel$clear()
            }
            
            parent$setSelected(TRUE)
            return(obj)
          })

### need to figure this out
## return the path in values
setMethod("[",
          signature(x="gTreeQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, guiToolkit("Qt"), i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gTreeQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            paths <- svalue(x, drop=FALSE, index=FALSE)
            if(length(paths) == 0)
              return(c())
            if(!missing(i))
              paths <- lapply(paths, function(a) a[i])
            
            if(length(paths) == 1)
              paths <- paths[[1]]

            return(paths)
          })

setMethod(".names",
          signature(toolkit="guiWidgetsToolkitQt",x="gTreeQt"),
          function(x, toolkit) {
            tr <- getWidget(x)
            headerItem <- tr$headerItem()
            n <- tr$columnCount
            if(n >= 1)
              sapply(seq_along(n), function(i) headerItem$text(i-1))
            else
              character(0)
          })
                     

setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x="gTreeQt"),
                 function(x, toolkit, value) {
                   ## do we set the headers
                   tr <- getWidget(x)
                   if(length(value) == tr$columnCount)
                     tr$setHeaderLabels(value)
                   else
                     warning(gettext("Names of incorrect length\n"))
                   return(x)
                 })
  
                   
## handler methods
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTreeQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerdoubleclick(obj, toolkit, handler, action, ...)
          })



f <- function(item, column, h) {
  tr <- getWidget(h$obj)
  h$column <- column + 1
  h$path <- .gtree.getItemPath(tr, item)
  h$handler(h)
}


## itemActivated might be double click!
## this is OS dependent.
## The advantage is this works with the keyboard too.
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTreeQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            h <- list(obj=obj, action=action, handler=handler)
            id <- qconnect(getWidget(obj), "itemActivated", f, user.data=h)
            invisible(id)
          })
setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTreeQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            h <- list(obj=obj, action=action, handler=handler)
            id <- qconnect(getWidget(obj), "itemDoubleClicked", f, user.data=h)
            invisible(id)
          })

## for dnd one might want itemEntered
