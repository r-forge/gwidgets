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

##' ##'
##' Constructor for gbigtable widget
##'
##" gbigtable is for large tables. It uses the qdataFrameModel widget for dataframes that is
##' much faster, but does not do things like icons,...
##' 
##' XXX This isn't working perfectly --
##' * setting selection isn't working
##' *  addHandlerChanged will crash if done enough times

gbigtable <- function(
                      items = NULL,
                      handler=NULL,
                      action=NULL,
                      container = NULL,
                      ... ,
                      toolkit=guiToolkit()
                      ) {
  widget <- .gbigtable (toolkit, items=items,
                        handler, action,
                        container=container ,...) 
  obj <- new( 'guiComponent', widget=widget, toolkit=toolkit) 
  return(obj)
}

#' class for the widget
setClass("gBigTableQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )



#' generic for toolkit dispatch
setGeneric( '.gbigtable' ,
           function(toolkit,
                    items = NULL,
                    handler=NULL,
                    action=NULL,
                    container = NULL, ... )
           standardGeneric( '.gbigtable' ))


#' gWidgetsQt interface to constructor
setMethod(".gbigtable",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   items=NULL,
                   handler=NULL,
                   action=NULL,
                   container=NULL,...)  {
            force(toolkit)

            ## setup view
            tbl <- Qt$QTableView()

            ## alternate shading
            tbl$setAlternatingRowColors(TRUE)
            ## header stuff
            header <- tbl$horizontalHeader()
            header$setStretchLastSection(TRUE) # stretch last to fill
            header$setClickable(TRUE)           # column clicks

            
            obj <- new("gBigTableQt", block=tbl, widget=tbl,
              toolkit=toolkit, ID=getNewID(), e = new.env())

            if(!missing(items))
              .leftBracket(obj, toolkit) <-  items

            if(!is.null(handler))
              .addhandlerchanged(obj, toolkit, handler=handler, action=action)
            
            if(!is.null(container)) 
              add(container, obj, ...)

            return(obj)
          })


#' return selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, index=NULL, drop=NULL,...) {
            object <- getWidget(obj)
            model <- object$model()
            if(is.null(model))
              return(NULL)
            df <- qdataFrame(model)
            df <- .unmungeDataFrame(df)
            
            selModel <- object$selectionModel()
            selectedItems <- selModel$selectedIndexes()
            selection <- sapply(selectedItems, function(item) c(row=item$row() + 1, column=item$column()+1))
            if(length(selection) == 0) {
              ## no selection
              return(NULL)
            }

            sel <- list(rows=sort(unique(selection[1,])),
                        columns=sort(unique(selection[2,]))
                        )
            ## adjust for visible rows
            ## the selection includes all rows
            visRows <- visible(obj)
            rowSelected <- rep(FALSE,dim(df)[1])
            rowSelected[sel$rows] <- TRUE
            sel$rows <- which(visRows & rowSelected)

            
            index <- getWithDefault(index, FALSE)
            drop <- getWithDefault(drop, TRUE)
            
                               
            if(index) {                 # do drop?
              return(sel)
            } else {
              if(drop)
                return(df[sel$rows, sel$columns]) # use columns here
              else 
                return(df[sel$rows, ])
            }
          })

#' set selection. Selections blocks only
#' value can be vector -- sets rows
#' list of rows, or rows and columns
#' XXX Needs to have a method implemented -- setSelection?
setReplaceMethod(".svalue",
                signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
                function(obj, toolkit, index=NULL, ..., value) {
                  object <- getWidget(obj)
                  n <- dim(obj)[2]
                  if(n < 1)
                    return(obj)         # nothing to do
                  
                  if(!is.list(value)) {
                    value <- list(value, 1:n)
                  }
                  
                  model <- object$model()
                  selModel <- object$selectionModel()

                  ## clear out
                  selModel$clear()

                  ## add
                  selection <- selModel$selection()
                  topLeft <- model$index(min(value[[1]])-1, min(value[[2]])-1)
                  bottomRight <- model$index(max(value[[1]])-1, max(value[[2]])-1)
                  selection$select(topLeft, bottomRight)
                  selModel$select(selection, Qt$QItemSelectionModel$Select)
                  return(obj)
                })
##' Lists the visible rows as a logical vector
setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, set=TRUE, ...) {
            d <- dim(obj)
            if(is.null(d) || d[1] == 0)
              return(logical(0))
            
            w <- getWidget(obj)
            visible <- sapply(1:d[1], function(i) !w$isRowHidden(i - 1))
            return(visible)
          })

##' set visible rows using a logical vector
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
                 function(obj, toolkit, ..., value) {
                   d <- dim(obj)
                   if(is.null(d) || d[1] == 0)
                     return(obj)

                   value <- rep(value, length=d[1]) # recycle!
                   
                   ## now redraw
                   w <- getWidget(obj)
                   sapply(seq_along(value), function(i) w$setRowHidden(i-1, !value[i]))

                   return(obj)
                 })


## data frame methods
##' [
setMethod("[",
         signature(x="gBigTableQt"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j,..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            object <- getWidget(x)
            model <- object$model()
            if(is.null(model))
              return(NULL)
            df <- qdataFrame(model)
            df <- .unmungeDataFrame(df)
            
            if(missing(i) && missing(j))
              df[,,drop=drop]
            else if (missing(j))
              df[i,,drop=drop]
            else if(missing(i))
              df[,j, drop=drop]
            else
              df[i,j, drop=drop]
          })


## [<-
setReplaceMethod("[",
                 signature(x="gBigTableQt"),
                 function(x, i, j,..., value) {
                   x <- .leftBracket(x, x@toolkit, i, j,..., value)
                  return(x)
                 })

##' function to unmunge the data frame
##' returns all variables without inital . in name
.unmungeDataFrame <- function(df) {
  df[, !grepl("\\.", names(df))]
}

##'
##' function to munge a data frame --
##' add in columns for alignments, etc.
.mungeDataFrame <- function(df) {

  sapply(names(df), function(varname) {
    var <- df[[varname]]
    
    if(is.numeric(var)) {
      ## alignment
      df[[sprintf(".%s.textAlignment", varname)]] <<- Qt$Qt$AlignCenter
    } else if(is.factor(var)) {
      ## factors are an issue, as no as.character coercion
      df[[sprintf(".%s.textAlignment", varname)]] <<- Qt$Qt$AlignRight
      df[[sprintf(".%s.background", varname)]] <<- sapply(seq_len(nrow(df)),
                                                          function(i) Qt$QBrush(Qt$QColor("yellow")))
    } else if(is.logical(var)) {
      df[[sprintf(".%s.textAlignment", varname)]] <<- Qt$Qt$AlignCenter
    } else {
      df[[sprintf(".%s.textAlignment", varname)]] <<- Qt$Qt$AlignLeft | Qt$Qt$AlignTop
    }
  })
  ## return data frame
    df
  
}

##' Method for [<-
##' really needs to have check on column type
setReplaceMethod(".leftBracket",
                 signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
                 function(x, toolkit, i, j, ..., value) {
                   object <- getWidget(x)
                   model <- object$model()

                   if(is.null(model) || (missing(i) && missing(j))) {
                     df <- .mungeDataFrame(value)
                   } else {
                     ## replace part
                     df <- qdataFrame(model) # munged
                     df <- .unmungeDataFrame(df)

                     if(missing(i))
                       i <- seq_len(dim(x)[1])
                     if(missing(j))
                       j <- seq_len(dim(x)[2])
                     ## logical to index
                     if(is.logical(i)) 
                       i <- seq_along(i)[i]
                     if(is.logical(j))
                       j <- seq_along(j)[j]
                     

                     df[i, j] <- value
                     df <- .mungeDataFrame(df)
                   }

                   model <- qdataFrameModel(df)
                   object$setModel(model)
                   
                   return(x)
                 })

##' dim must first unmunge
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
          function(x,toolkit) {
            object <- getWidget(x)
            model <- object$model()
            if(is.null(model))
              return(NULL)
            df <- qdataFrame(model)
            df <- .unmungeDataFrame(df)
            dim(df)
          })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
          function(x,toolkit) {
            d <- dim(x)
            return(d[2])
          })


setMethod(".dimnames",
          signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
          function(x, toolkit) {
            object <- getWidget(x)
            model <- object$model()
            if(is.null(model))
              return(NULL)
            df <- qdataFrame(model)
            df <- .unmungeDataFrame(df)
            dimnames(df)
          })

setReplaceMethod(".dimnames",
                 signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
                 function(x, toolkit,  value) {
                   ## get df
                   object <- getWidget(x)
                   model <- object$model()
                   if(is.null(model))
                     return(NULL)
                   df <- qdataFrame(model)
                   df <- .unmungeDataFrame(df)

                   dimnames(df) <- value

                   ## df -> model
                   df <- .mungeDataFrame(df)
                   model <- qdataFrameModel(df)
                   object$setModel(model)
                   x
                 })



setMethod(".names",
          signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
          function(x, toolkit) {
            dimnames(x)[[2]]
          })


setReplaceMethod(".names",
                 signature(toolkit="guiWidgetsToolkitQt",x="gBigTableQt"),
                 function(x, toolkit, value) {
                   ## object -> df
                   object <- getWidget(x)
                   model <- object$model()
                   if(is.null(model))
                     return(NULL)
                   df <- qdataFrame(model)
                   df <- .unmungeDataFrame(df)
                   
                   names(df) <- value

                   ## df -> object
                   df <- .mungeDataFrame(df)
                   model <- qdataFrameModel(df)
                   object$setModel(model)
                   x
                   x
                 })

## handlers to add
##' Handlers all return row and column along with obj and action
.addHandlergBigTableQt <- function(obj,  signal,  handler, action=NULL) {
  f <- function(item, h) {
    h$row <- item$row() + 1
    h$column <- item$column() + 1
    handler(h)
  }
  h <- list(obj=obj, action=action)
  ID <- qconnect(getWidget(obj), signal, f, user.data=h)
  invisible(ID)
}
## really selection changed
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            sel <- getWidget(obj)$selectionModel()
            f = function(sel, unsel, h) {
              handler(h)
            }
            h <- list(obj=obj, action=action)
            ID <- qconnect(sel, "selectionChanged", f, user.data=h)
            invisible(ID)
          })
          
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandlergBigTableQt(obj, "clicked", handler, action)
          })

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandlergBigTableQt(obj, "doubleClicked", handler, action)
          })

setMethod(".addhandlercolumnclicked",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            object <- getWidget(obj)
            header <- object$horizontalHeader()

            f <- function(ind, h) {
              h$column <- ind + 1
              handler(h)
            }
            h <- list(obj=obj, action=action)
            ID <- qconnect(header, "sectionClicked", f, user.data=h)
            invisible(ID)
            
          })


setMethod(".addhandlercolumndoubleclick",
          signature(toolkit="guiWidgetsToolkitQt",obj="gBigTableQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            object <- getWidget(obj)
            header <- object$horizontalHeader()

            f <- function(ind, h) {
              h$column <- ind + 1
              handler(h)
            }
            h <- list(obj=obj, action=action)
            ID <- qconnect(header, "sectionDoubleClicked", f, user.data=h)
            invisible(ID)
            
          })

