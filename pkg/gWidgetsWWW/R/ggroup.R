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

## Box Containers for gWidgetsWWW
##
## The container code in Ext is very similar.
## All use Ext.Panel with some different configuration options
## this code tries to take advantage of that  by introducing some sub"traits".
## the tricky thing is the call up to ExtCfgOptions from the inherited trait.
## just calling with .super didn't work

EXTPanel <- EXTContainer$new()
EXTPanel$ExtConstructor <- "Ext.Panel"



EXTGroup <- EXTPanel$new(children=list())
EXTGroup$ExtCfgOptions <-  function(.) {
  out <- list(
              border = FALSE,
              hideBorders=TRUE,          # key to getting rid of blud
              collapsed=!.$..visible
              )
  ## We consulted:
  ## http://stackoverflow.com/questions/2479342/problem-with-extjs-vbox-layout-nested-in-a-hbox-layout
  ## There had been an issue and for some odd reason these argument
  ## were a bit fussy -- put in the wrong ones and thewhole thing
  ## blows up. Anyways, these seem to work, although there are issues with some nested ggroup containers.
  if(exists("..horizontal",envir=., inherits=FALSE)) {


    frame <- !is.null(getOption("gWidgetsWWW_debug"))

    if(.$..horizontal) {

      ## was
#      out[['layout']] <- "column"

      out[['layout']] <- "hbox"
      out[['defaults']] <- list(frame=frame, defaultAnchor="t", flex=0)
      
      if(exists("..use.scrollwindow", envir=., inherits=FALSE))
        out[['autoScroll']] <- .$..use.scrollwindow
    } else {

      out[['layoutConfig']] <-  list(type="vbox", pack="start") #, align="stretch")
      out[['defaults']] <- list(flex=0, defaultAnchor="l", frame=frame)

      
      ## This is  wierd -- vbox fails here. What is right for vertical layout
#      out[['layout']] <- "vbox"
#      out[['autoscroll']] <- TRUE
    }
  }
  ## size
  ## out[['autoWidth']] <- TRUE
  if(exists("..width", envir = ., inherits =FALSE))
    out[["width"]] <- .$..width
  if(exists("..height", envir = ., inherits =FALSE))
    out[["height"]] <- .$..height
  spacing <- 10
  if(exists("..spacing", envir=., inherits=FALSE)) spacing <- .$..spacing
##  out[['spacing']] <- spacing
#  out[["bodyStyle"]] = String('{') + 'padding:"' + spacing + 'px"}'
    out[["bodyStyle"]] = list('padding' = String('"') + spacing + 'px"'  )
  return(out)
}
## even group can be made visible/hidden
EXTGroup$setVisibleJS <- function(.) {
  if(exists("..setVisibleJS", envir=., inherits=FALSE))
    .$..setVisibleJS()
  if(.$..visible)
    .$callExtMethod("expand","true")
  else
    .$callExtMethod("collapse","true")
}

EXTGroup$addSpace <- function(., value, horizontal=TRUE, ...) {
  n <- ceiling(value/8)
  ghtml(paste(rep("&nbsp", n), sep="", collapse=""), cont=.)
}
## this is not defined
EXTGroup$addSpring <- function(.) {invisible("")}

##' ggroup is the basic box container
##'
##' Basic box container.
##' Warning: When groups are nested, it may be necessary to
##' set the width of a horizontal box container, as otherwise sibling
##' components to the right of the container will not be displayed.
##' @param horizontal logical. If True a hbox, else a vbox
##' @param spacing spacing between child components
##' @param use.scrollwindow ignored
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @example Tests/test-ggroup.R
##' @export
ggroup <- function(horizontal=TRUE, spacing=5, use.scrollwindow = FALSE,
                    container,...) {
   ## a group
  cont <- EXTGroup$new(toplevel = container$toplevel,
                     ..horizontal = horizontal,
                     ..spacing = spacing,
                     ..use.scrollwindow = use.scrollwindow
                     )
  cont$..visible <- TRUE
  
  theArgs <- list(...)
  if(!is.null(theArgs$width)) cont$..width <- theArgs$width
  if(!is.null(theArgs$height)) cont$..height <- theArgs$height

  class(cont) <- c("gGroup",class(cont))
   container$add(cont,...)
   invisible(cont)
 }

##################################################
EXTFrame <- EXTGroup$new(children=list())
EXTFrame$ExtCfgOptions <- function(.) {
  out <- EXTGroup[['ExtCfgOptions']](.)
  out[['title']] <- escapeHTML(svalue(.))
  return(out)
}
EXTFrame$setValueJSMethod = "setTitle"

##' gframe is a title-decorated ggroup box container
##'
##' Use \code{svalue<-} to adjust the title
##' @param text label text
##' @param pos position of label. Ignored?
##' @param horizontal logical. A hbox or vbox?
##' @param container parent container
##' @param ... passed to add method of parent
##' @rdname ggroup
gframe <- function(text = "", pos = 0, horizontal=TRUE, container=NULL,...) {

  cont <- EXTFrame$new(toplevel = container$toplevel,
                    ..horizontal = horizontal)
                    
  cont$..visible <- TRUE

  theArgs <- list(...)
  if(!is.null(theArgs$width)) cont$..width <- theArgs$width
  if(!is.null(theArgs$height)) cont$..height <- theArgs$height

  
  class(cont) <- c("gFrame",class(cont))
  cont$setValue(value=text)
  cont$..pos <- pos

  cont$..ExtCfgOptions <- function(.)
    list(border=TRUE)
  
  container$add(cont,...)  
  invisible(cont)
}

##################################################
EXTExpandGroup <- EXTFrame$new(children=list())
EXTExpandGroup$ExtCfgOptions <- function(.) {
  out <- .super$ExtCfgOptions(.)
  out[['collapsible']] <- TRUE
  out[['titleCollapse']] <- TRUE
  return(out)
}

##' gexpandgroup is a group with trigger icon and label
##'
##' Use \code{svalue<-} to adjust the title. The \code{visible<-}
##' method is used to programatically change display
##' @param text label text
##' @param horizontal logical. Indicates direction children are added
##' @param handler Called when expanded or closed
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of parent
##' @rdname ggroup
##' @export
gexpandgroup <- function(text = "", horizontal = TRUE,
                         handler = NULL, action=NULL,
                         container=NULL, ...) {

  cont <- EXTExpandGroup$new(toplevel=container$toplevel,
                            ..horizontal=horizontal)
  cont$..visible <- TRUE
  
  theArgs <- list(...)
  if(!is.null(theArgs$width)) cont$..width <- theArgs$width
  if(!is.null(theArgs$height)) cont$..height <- theArgs$height

  class(cont) <- c("gExpandgroup",class(cont))
  cont$setValue(value=text)

  cont$..ExtCfgOptions <- function(.) {
    out <- list(border=TRUE)
    out
  }

  if(!is.null(handler))
    addHandlerClicked(cont, handler = handler, action=action)

  container$add(cont,...)    
  invisible(cont)
}
  
