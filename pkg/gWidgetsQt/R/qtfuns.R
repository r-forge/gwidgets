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

## QT helper functions to be shared by widget

## Character width
QtCharacterWidth <- 9 ## change to corrct?


## flags


## return Qt widget from obj
## ** This should be a method **
getWidget = function(obj) {
  if(is(obj,"RQtObject"))
    return(obj)

  ## recurse until we are done
  if(is(obj,"gWidgetQt"))
    return(getWidget(obj@widget))
  else if(is(obj,"guiWidget"))
    return(getWidget(obj@widget))
  else
    return(NA)
}

getBlock = function(obj) {
  if(is(obj,"RQtObject")) return(obj)
  if(is(obj,"gWidgetQt"))
    return(getBlock(obj@block))
  else if(is(obj,"guiWidget"))
    return(getBlock(obj@widget))
  else
    return(NA)
}


getTopParent = function(obj) {
  ## in env is parent variable if present
  ans <- NULL
  
  while(is.null(ans)) {
    e <- obj$env$parent
    if(is.list(e)  &&
       e[['ID']] =="")
      ans <- obj
    else obj <- obj$env$parent
  }
  return(ans)
}


setMethod(".getToolkitWidget",
          signature(obj="gWidgetQt", toolkit="guiWidgetsToolkitQt"),
          function(obj, toolkit) getWidget(obj))




## Does the top level window exists
windowExists = function(obj) {
  win = getTopParent(getWidget(obj))
  XXX("What to do with win in WindowExists")
}


##################################################
## Fonts
makeQFont <- function(font.attr) {
  f <- Qt$QFont()

  ## process into a list
  if(is.list(font.attr))
    l <- font.attr
  else
    l <- lapply(font.attr, function(i) i)

  ## family
  if(!is.null(l$family)) {
    family <- switch(l$family,
                     "normal"="arial",
                     "sans" = "arial",
                     "serif" = "times",
                     "monospace" = "courier",
                     l$family)
    f$setFamily(family)
  }
  ## sizse
  if(!is.null(l$size)) {
    if(is.numeric(l$size)) {
      val <- as.integer(l$size)
    } else {
      val <- switch(l$size,
                    "xx-large" = 24L,
                    "x-large" = 20L,
                    "large" = 16L,
                    "medium" = 14L,
                    "small" = 12L,
                    "x-small" = 10L,
                    "xx-small" = 8L,
                    12L
                    )
    }
    f$setPixelSize(val)
      
  }
  ## style
  if(!is.null(l$style)) {
    QtStyles <- c("normal"=Qt$QFont$StyleNormal,
                  "italic"=Qt$QFont$StyleItalic,
                  "oblique"=Qt$QFont$StyleOblique)
    if(l$style %in% names(QtStyles))
      f$setStyle(QtStyles[l$style])
  }
  ## weight
  if(!is.null(l$weight)) {
    QtWeights = c(
      "ultra-light"=0L,
      "light" = Qt$QFont$Light,
      "normal"=Qt$QFont$Normal,
      "bold"=Qt$QFont$Bold,
      "ultra-bold" = Qt$QFont$Black,
      "heavy" = 99L)
    if(l$weight == "bold")
      f$setBold(TRUE)
    else if(l$weight %in% QtWeights)
      f$setWeight(QtWeights[l$weight])
  }
  return(f)
}

makeQTextCharFormat<- function(font.attr) {
  tcf <- Qt$QTextCharFormat()
  tcf$setFont(makeQFont(font.attr))
  tcf
}


##################################################
### gdf and gtable
## some helper functions for gtable and gdf
## format a colmn in a table
formatColumn <- function(x, j, tbl, pattern) UseMethod("formatColumn")
formatColumn.default <- function(x, j, tbl, pattern) tbl
formatColumn.integer <- function(x, j, tbl, pattern) {
  lapply(1:length(x), function(i) {
    item <- tbl$item(i-1, j-1)
    item$setTextAlignment(Qt$Qt$AlignRight)
  })
  invisible()
}

formatColumn.numeric <- function(x, j, tbl, pattern) {
  if(missing(pattern))
    pattern <- "%.5f"
  lapply(1:length(x), function(i) {
    item <- tbl$item(i-1, j-1)
    item$setTextAlignment(Qt$Qt$AlignRight)            # right
    item$setText(sprintf(pattern, x[i]))
  })
  invisible()
}



## set Table cell
##' this saves some typing
setTableWidgetCell <- function(tbl, x, i, j, flags = c("selectable", "editable", "enabled")) {
  QtTableItemFlags <- list(selectable=Qt$Qt$ItemIsSelectable,
                        editable=Qt$Qt$ItemIsEditable, 
                        dragEnabled=Qt$Qt$ItemIsDragEnabled,
                        dropEnabled=Qt$Qt$ItemIsDropEnabled,
                        userCheckable=Qt$Qt$ItemIsUserCheckable,
                        enabled=Qt$Qt$ItemIsEnabled,
                        tristate=Qt$Qt$ItemIsTristate)

  item <- Qt$QTableWidgetItem(as.character(x))
  if(length(flags))
    item$setFlags(Reduce("|", QtTableItemFlags[flags]))
  tbl$setItem(i-1, j-1, item)
}

## set column
## need to format
setTableWidgetColumn <- function(tbl, x, j, flags=c("selectable", "editable", "enabled")) {
  M <- length(x)
  lapply(1:M, function(i) {
    setTableWidgetCell(tbl, x[i], i, j, flags=flags)
  })
}

## populate table
## does formatting
setTableWidgetFromDataFrame <- function(tbl, df, flags=c("selectable", "editable", "enabled")) {
  M <- nrow(df); N <- ncol(df)

  tbl$clear()

  if(N==0)
    return()

  
  ## resize to fit the number of rows, column
  tbl$setRowCount(M)
  tbl$setColumnCount(N)

  ## put in values
  lapply(1:N, function(j) {
    setTableWidgetColumn(tbl, df[,j], j, flags)
  })

  ## format the columns
  lapply(1:N, function(j) formatColumn(df[,j], j, tbl))
  
  ## set row and col names
  setTableWidgetNames(tbl, colnames(df))
  setTableWidgetRowNames(tbl, rownames(df))

  invisible()
}

setTableWidgetIcons <- function(tbl, icons) {
  ## icons are same length as table row count -- no recycling
  if(tbl$rowCount != length(icons)) {
    ### "Wrong length for icons"
    return()
  }
  lapply(1:tbl$rowCount, function(i) {
    icon <- getStockIconFromName(icons[i])
    item <- tbl$item(i-1, 0)
    if(!is.null(icon))
      item$setIcon(icon)
  })
}

setTableWidgetNames <- function(tbl, nms) {
  N <- tbl$columnCount
  sapply(1:N, function(j) {
    item <- tbl$horizontalHeaderItem(j - 1)
    if(!is.null(item)) {
      item$setText(nms[j])
    } else {
      item <- Qt$QTableWidgetItem(nms[j])
      tbl$setHorizontalHeaderItem(j - 1, item)
    }      
  })
}

getTableWidgetNames <- function(tbl) {
  N <- tbl$columnCount
  sapply(1:N, function(j) {
    item <- tbl$horizontalHeaderItem(j-1)
    if(!is.null(item))
      item$text()
    else
      sprintf("X.%s",j)
  })
}
setTableWidgetRowNames <- function(tbl, nms) {
  M <- tbl$rowCount
  sapply(1:M, function(i) {
    item <- tbl$verticalHeaderItem(i - 1)
    if(!is.null(item)) {
      item$setText(nms[i])
    } else {
      item <- Qt$QTableWidgetItem(nms[i])
      tbl$setVerticalHeaderItem(i - 1, item)
    }
  })
}

getTableWidgetRowNames <- function(tbl) {
  M <- tbl$rowCount
  sapply(1:M, function(i) {
    item <- tbl$verticalHeaderItem(i-1)
    if(!is.null(item))
      item$text()
    else
      i
  })
}

## get value as data frame
getValueFromTableWidget <- function(tbl, colClasses=c("character")) {

  M <- tbl$rowCount; N <- tbl$columnCount
  if(N == 0) {
    return(data.frame(character(0), stringsAsFactors=FALSE))                        # no columns
  } else if(M == 0) {
    colClasses <- rep(colClasses, length=N)        # recycle
    d <- as.data.frame(lapply(1:N, function(i) character(0)), stringsAsFactors=FALSE)
    for(j in 1:N) 
      d[,j] <- if(colClasses[j] == "factor") d[,j] else as(d[,j], colClasses[j])
    return(d)
  }

  ## carry on
  out <- matrix(character(M*N), nrow=M)
  colClasses <- rep(colClasses, length=N)        # recycle
  
  lapply(1:N, function(j) {
    out[,j] <<- sapply(1:M, function(i) {
      item <- tbl$item(i-1, j-1)
      item$text()
    })
  })
  df <- as.data.frame(lapply(1:N, function(j) {
    if(colClasses[j] == "factor")
      factor(out[,j])
    else
      as(out[,j], colClasses[j])
  }), stringsAsFactors=FALSE)

  colNames <- sapply(1:N, function(j) {
    item <- tbl$horizontalHeaderItem(j-1)
    if(is.null(item))
      sprintf("X.%s",j)
    else
      item$text()
  })
  names(df) <- make.names(colNames)

  rowNames <- sapply(1:M, function(i) {
    tbl$verticalHeaderItem(i-1)$text()
  })
  rownames(df) <- rowNames
  
  return(df)
}


## return rows, columns of selection as matrix with each selection, or turns into rectangular block
getTableWidgetSelection <- function(tbl, as.rectangle=FALSE) {
  ## must be a faster way
  selectedItems <- tbl$selectedItems()
  if(length(selectedItems) == 0) {          # no selection
    return(NULL)
  }

  selection <- sapply(selectedItems, function(item) c(row=item$row(), column=item$column()))
  if(as.rectangle) {
    rows <- selection[1,]
    columns <- selection[2,]
    rows <- sort(unique(rows))
    columns <- sort(unique(columns))
    list(rows=rows+1, columns=columns+1)
  } else {
    selection
  }
}

## set selection. 
## rows and columns are 1-based -- not 0-bsed
setTableWidgetSelection <- function(tbl, rows, columns, select=TRUE, clear=TRUE, full.row=FALSE) {
  if(!full.row && missing(columns)) {
    if(is.list(rows)) {
      columns <- rows[[2]]; rows <- rows[[1]]
    } else {
      return()                          # need both row, col
    }
  }
  if(clear) {
    ## remove/ otherwise will add
    m <- tbl$selectionModel()
    m$clearSelection()
  }

  ## values are 1-base, so we subtract 1
  ## we loop -- useing the selectino range isn't working
  if(full.row) {
    for(i in rows) 
      tbl$setCurrentCell(i - 1, 0, Qt$QItemSelectionModel$Rows |  Qt$QItemSelectionModel$Select)
  } else {
    for(i in rows) 
      for(j in columns)
        tbl$setCurrentCell(i - 1, j - 1, Qt$QItemSelectionModel$Select)

  ## rng = Qt$QTableWidgetSelectionRange(top, left, bottom, right)
  ## tbl$setRangeSelected(rng, select)


  } 

}


##' Helper function to find a stock icon for an object
##' This should be gWidgets!
findStockIcon <- function(x) UseMethod("findStockIcon")
findStockIcon.default <- function(x) Qt$QIcon(system.file("images/symbol_square.gif", package="gWidgets"))
findStockIcon.numeric <- function(x) Qt$QIcon(system.file("images/numeric.gif", package="gWidgets"))
findStockIcon.integer <- function(x) Qt$QIcon(system.file("images/numeric.gif", package="gWidgets"))
findStockIcon.character <- function(x) Qt$QIcon(system.file("images/character.gif", package="gWidgets"))
findStockIcon.factor <- function(x) Qt$QIcon(system.file("images/factor.gif", package="gWidgets"))
findStockIcon.data.frame <- function(x) Qt$QIcon(system.file("images/dataframe.gif", package="gWidgets"))
findStockIcon.matrix <- function(x) Qt$QIcon(system.file("images/matrix.gif", package="gWidgets"))
findStockIcon.function <- function(x) Qt$QIcon(system.file("images/function1.gif", package="gWidgets"))
findStockIcon.list <- function(x) Qt$QIcon(system.file("images/datasheet.gif", package="gWidgets"))
findStockIcon.ts <- function(x) Qt$QIcon(system.file("images/ts.gif", package="gWidgets"))
findStockIcon.lm <- function(x) Qt$QIcon(system.file("images/graph.gif", package="gWidgets"))
findStockIcon.posixCt <- function(x) Qt$QIcon(system.file("images/date.gif", package="gWidgets"))
