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

## use gformlayout from gWidgets -- which was inspired by extjs fieldset
## changes should synchronize with gWidgets file

## helper functions
.makeForm <- function(., lst, parent, ...) {
  g <- ggroup(cont = parent, expand=TRUE,...)

  ## make a local copy of lst and modify for do.call
  tmp <- lst;
  tmp$name <- tmp$type <- tmp$children <- NULL
  tmp$depends.on <- tmp$depends.FUN <- tmp$depends.signal <- NULL
  tmp$container <- g; tmp$expand <- TRUE
  ## expand functions
  for(i in names(tmp)) {
    if(is.function(tmp[[i]]))
      tmp[[i]] <- tmp[[i]]()
  }
  
  ## treat fieldset differently
  if(lst$type == "fieldset") {
    .$makeFieldset(lst, g, label = lst$label, width=lst$width, height=lst$height)
    return()
  } else {  
    ## make object
    ## evaluate quoted functions
    tmp <- lapply(tmp, function(i) if(is.name(i)) eval(i)() else i)
    newObject <- do.call(lst$type, tmp)
    ## store if a name is given
    if(!is.null(lst$name)) {
      tmp <- .$..widgets
      tmp[[lst$name]] <- newObject
      .$..widgets <- tmp
    }
    ## do we enable new object
    if(!is.null(lst$depends.on)) {
      widget <- .$..widgets[[lst$depends.on]]
      if(is.null(lst$depends.signal))
        lst$depends.signal <- "addHandlerChanged"
      do.call(lst$depends.signal,list(obj=widget, handler =  function(h,...) {
        value <- svalue(h$obj)
        enabled(newObject) <- lst$depends.FUN(value)
      }))
      enabled(newObject) <- lst$depends.FUN(svalue(widget))
    }
  }
   


  
  ## show children if there
  ## this recurses except on "fieldset"
  if(!is.null(lst$children)) {
    for(i  in 1:length(lst$children)) {
      l <- lst$children[[i]]
      if(l$type == "fieldset") {
        if(lst$type == "gnotebook")
          .$makeFieldset(l, newObject, label = l$label)
        else
          .$makeFieldset(l, newObject, width = l$width, height = l$height)
      } else {
        if(lst$type == "gnotebook")
          .$makeForm(l, newObject, label = l$label)
        else
          .$makeForm(l, newObject)
      }
    }
  }
}


## fieldset does not recurse
.makeFieldset <- function(., lst, parent, width=NULL, height=NULL, ...) {
  ## parent is parent container
  ## lst is list as above

  
  ## outer container
  if(!is.null(lst$label)) 
    g <- gframe(lst$label, cont=parent, width=width, height=height,...)
  else
    g <- ggroup(cont=parent,  width=width, height=height, ...)
  ## main table
  tbl <- glayout(cont = g)
  
  ## do we enable new object
  if(!is.null(lst$depends.on)) {
    widget <- .$..widgets[[lst$depends.on]]
    if(is.null(lst$depends.signal))
      lst$depends.signal <- "addHandlerChanged"
    do.call(lst$depends.signal, list(obj = widget,handler = function(h,...) {
      value <- svalue(h$obj)
      enabled(g) <- lst$depends.FUN(value)
    }))
    enabled(g) <- lst$depends.FUN(svalue(widget))
  }
  
  ## fix label adjust
  if(is.null(lst$label.pos))
    lst$label.pos <- "left"
  if(lst$label.pos == "top") {
    label.anchor <- c(-1,0)
  } else {
    if(is.null(lst$label.just) || lst$label.just == "left")
      label.anchor <- c(-1,1)
    else if(lst$label.just == "center")
      label.anchor <- c(0,1)
    else
      label.anchor <- c(1,1)
  }
  
  if(is.null(lst$columns)) 
    no.columns <- 1
  else
    no.columns <- lst$columns
  
  ## add children
  for(i in 1:length(lst$children)) {
    l <- lst$children[[i]]
    ## each child is a list with name, label, type, then arguments
    ## make new list for do.call
    tmp <- l;
    tmp$name <- tmp$label <- tmp$type <- NULL
    tmp$depends.on <- tmp$depends.FUN <- tmp$depends.signal <- NULL
    tmp$container <- tbl

    newWidget <- do.call(l$type, tmp)

    ## store
    if(!is.null(l$name)) {
      tmp <- .$..widgets
      tmp[[l$name]] <- newWidget
      .$..widgets <- tmp
    }
    ## do we enable new object
    if(!is.null(l$depends.on)) {
      widget <- .$..widgets[[l$depends.on]]
      if(is.null(l$depends.signal))
        l$depends.signal <- "addHandlerChanged"
      do.call(l$depends.signal, list(obj = widget, handler =  function(h,...) {
        value <- svalue(h$obj)
        enabled(newWidget) <- l$depends.FUN(value)
      }))
      enabled(newWidget) <- l$depends.FUN(svalue(widget))
    }

    
    ## add to table
    col <- 1 + (i - 1) %% no.columns    #1, ..., no.columns
    row <- 1 + (i - 1) %/% no.columns   #1, ...
    newLabel <- glabel(l$label, cont = tbl)
    if(!is.null(lst$label.font))
      font(newLabel) <- lst$label.font
    if(is.null(lst$label.pos) || lst$label.pos == "left") {
      tbl[row, 2 * (col - 1) + 1, anchor=label.anchor] <- newLabel
      if(l$type %in% c("gcombobox","gdroplist"))
        tbl[row, 2 * (col - 1) + 2, anchor=c(-1,1), expand=TRUE] <- newWidget
      else
        tbl[row, 2 * (col - 1) + 2, anchor=c(-1,1)] <- newWidget
    } else {
      tbl[2 * (row - 1) + 1, col, anchor=label.anchor] <- newLabel
      if(l$type %in% c("gcombobox","gdroplist"))
        tbl[2 * (row - 1) + 2, col, anchor=c(-1,1), expand=TRUE] <- newWidget
      else
        tbl[2 * (row - 1) + 2, col, anchor=c(-1,1)] <- newWidget
    }
  }
}



gformlayout <- function(lst, container = NULL, ...) {
  obj <- ggroup(cont = container, ...)
  g <- ggroup(cont = obj, expand=TRUE)

  class(obj) <- c("gFormLayout",class(obj))

  obj$..widgets <- list()
  obj$makeForm <- .makeForm
  obj$makeFieldset <- .makeFieldset

  obj$makeForm(lst, g)

  obj$getValue <- function(., index=NULL, drop=NULL)
    return(lapply(.$..widgets, svalue))
  obj$getNames <- function(.)
    return(names(.$..widgets))
  
  
  return(obj)
}
