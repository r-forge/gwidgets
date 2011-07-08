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


##' gtree widget
##'
##' Widget to create atree dynamically by specifying the offspring. This implementation
##' does not allow for grids.
##' @param offspring Function with signature (path, data). Returns a
##' data frame with columns: id (which create the path), hasOffspring
##' (logical indicating if it has children), optionally an icon (a
##' stock icon name) and values (since a grid is not used, these are
##' pasted onto the id string.) The path comes from the id values. Can
##' be updated through proto method \method{setOffspring}. The
##' id's must not have a ':' as that is chosen as a delimiter.
##' @param offspring.data passed to offspring call so that the offspring function can be parameterized if desired. Can be updated through proto method \method{setOffspringData}.
##' @param icon.FUN NULL, function or logical. If a function then will
##' compute icons from calling this on offspring, if logical (and
##' TRUE) then third column is assumed to be a stock icon, if NULL
##' then no icons.
##' @param chosencol (if/when?) grid is implemented will work with svalue method
##' @param multiple for multiple selection (not implemented)
##' @param handler called on double click
##' @param action passed to handler
##' @param container container object
##' @param ... passed to containers add method
##' @TODO implement multiple
##' @examples
##' \dontrun{
##' # galton watson
##' p <- .5 
##' offspring <- function(path, ...) {
##'   x <- rbinom(2, 1, p)
##'   nms <- paste(path, 0:1, sep=":")
##'   icons <- c("dismiss","ok")[x+1]
##'   data.frame(id=nms, hasoffspring=as.logical(x), icons=icons, stringsAsFactors=FALSE)
##' }
##' 
##' w <- gwindow("Galton Watson tree")
##' g <- ggroup(cont=w, horizontal=FALSE)
##' ghtml("A node in a Galton-Watson tree has 0 or 2 offspring.<br /> In this case each has equal chance.", cont=g)
##' gseparator(cont=g)
##' tr <- gtree(offspring=offspring, icon.FUN=TRUE, cont=g)
##' size(tr) <- c(300,300)
##' b <- gbutton("Try again", cont=g, handler=function(h,...) tr$update())
##' visible(w) <- TRUE
##' }
gtree <- function(offspring = NULL,
                  offspring.data = NULL,
                  icon.FUN = NULL,
                  chosencol = 1,
                  multiple = FALSE, 
                  handler = NULL, action = NULL,
                  container = NULL,
                  ...) {

  widget <- EXTComponentWithProxyTreeStore$new(toplevel=container$toplevel,
                                               ..multiple = multiple
                                               )

  class(widget) <- c("gTree",class(widget))

  theArgs <- list(...)
  
  ## set up store
  store <- EXTProxyTreeStore$new(toplevel=container$toplevel)
  store$ID <- container$newID()       # set ID
  container$toplevel$addStore(store)
  store$chosenCol <- chosencol

  store$..offspring <- offspring        
  store$..offspring.data <- offspring.data
  store$..icon.FUN <- icon.FUN
  store$..path <- character(0)
  widget$..store <- store

  
  widget$..data <- list(path="0", text=NA)             # base node with no value
  ## will need setValues method, ...

  
  widget$ExtConstructor <- "Ext.tree.TreePanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(useArrows=TRUE,
                autoScroll=TRUE,
                animate=TRUE,
                border=FALSE,
                enableDrag=TRUE,
                trackMouseOver=TRUE,
                rootVisible=FALSE,
                dataUrl=sprintf('%s/%s/%s/%s',
                  .$toplevel$..gWidgetsWWWAJAXurl,"proxystore", .$..store$asCharacter(), .$toplevel$sessionID),
                root=list(
                  expanded=TRUE,
                  nodeType='async',
                  draggable=FALSE,
                  id= '0'
                  )
                )
    

    
    ## size in panel config, not setStyle
    if(exists("..width",envir = .,inherits=FALSE))
      out[["width"]] <- .$..width
    else
      out[["width"]] <- "auto"
    
    if(exists("..height",envir = .,inherits=FALSE))
      out[["height"]] <- .$..height
    else
      out[["height"]] <- "auto"
    
    return(out)
  }

  ## what to do with this?
  widget$assignValue <- function(., value) {
    value <- value[[1]]                 # could tidy up
    .$..data <- list(path=strsplit(value[[1]], ":")[[1]][-1], text=value[[2]])
  }
  
  ## index=TRUE -- return path
  ## otherwise (default) return text of selected
  ## no means to return the whole path, but could get with offpring and a loop
  widget$getValue <- function(., index=NULL, drop=NULL, ...) {
    ## if(exists("..shown",envir=.,inherits=FALSE)) {
    ##   ## get from widget ID
    ##   out <- try(get(.$ID, envir=.$toplevel),silent=TRUE)
    ##   if(inherits(out,"try-error")) {
    ##     out <- .$..data
    ##   } else {
    ##     .$..data <- out                  # update data
    ##   }
    ## } else {
    ##   out <- .$..data
    ## }
    ## out is in form 0:path:text

    out <- .$..data

    index <- getWithDefault(index, FALSE)
    drop <- getWithDefault(drop, TRUE)

    if(index) {
      ind <- out[[1]]
      if(drop)
        tail(ind,n=1)
      else
        ind
    } else {
      out[[2]]
    }

  }

  ## update tree. Simply collapses values and when reexpanded will be all new
  widget$update <- function(x, ...) {
    . <- x
    if(.$has_local_slot("..shown")) {
      out <- String() +
        sprintf("%s.getRootNode().collapse();", .$asCharacter()) +
          sprintf("%s.getLoader().load(%s.getRootNode());", .$asCharacter(),.$asCharacter()) +
            sprintf("%s.getRootNode().expand();", .$asCharacter()) 
      .$addJSQueue(out)
    }
  }

  ## Can update after widget is shown through these proto methods
  widget$setOffspring <- function(., offspring) {
    .$..store$..offspring <- offspring
    .$update()
  }
  widget$setOffspringData <- function(., offspring.data)  {
    .$..store$..offspring.data <- offspring.data
    .$update()
  }
  
  ## XXX TODO: send back [path:path,text:text]

  widget$transportSignal <- c("click")
  widget$handlerArguments <- function(...) "node, e"
  widget$transportValue <- function(.,...) {
    ## we pass back both node and the text here
    out <- "var value = {id: node.id, text: node.text};"
    return(out)
  }

  ## add
  container$add(widget,...)

  ## changed = double clicked
  widget$addHandlerChanged <- function(., handler, action=NULL)
      .$addHandler(signal="dblclick",handler, action)

  if(!is.null(handler))
    widget$addHandlerChanged(handler, action=action)
  
  
  invisible(widget)
  
}
