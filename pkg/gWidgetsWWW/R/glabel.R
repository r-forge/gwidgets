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

##' A label widget
##' 
##' @param text text for label. If multiline will be joined with " " or "<br />" (when \code{markup=TRUE})
##' @param markup If \code{TRUE} then text can be HTML. Useful for
##' newlines. A value of \code{FALSE} will cause HTML constructs to be
##' escaped. This is useful if user-supplied values are being
##' displayed.
##' @param editable 
##' @param handler 
##' @param action 
##' @param container 
##' @param ... 
glabel <- function(text = "", markup = FALSE, editable = FALSE,
                   handler = NULL, action = NULL, container = NULL,...) {

  widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                             ..editable = editable,
                             ..markup = markup
                             )
  
  class(widget) <- c("gLabel",class(widget))
  if(!markup)
    text <- escapeHTML(text)
  widget$setValue(value=text)


  ## strip of \n so we can push thourgh '' in one line.
  
  widget$getValue <- function(.,...) paste(.$..data, collapse=ifelse(.$..markup, "<br />", " "))

  widget$setValueJSMethod = "setValue"
  ##' ensure we strip off \n values
  widget$setValueJS <- function(., ...) {
    out <- sprintf("%s.setValue('%s');", .$asCharacter(), stripSlashN(svalue(.), encode=!.$..markup, dostrwrap=FALSE))
    return(out)
  }
  widget$getValueJSMethod = "setValue"
  widget$ExtConstructor <- "Ext.ux.labelBox"
  widget$ExtCfgOptions <-  function(.) {
    out <- list(
                value=stripSlashN(svalue(.), encode=!.$..markup, dostrwrap=FALSE)
                ) ## was unescapeURL(svalue(.)
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
}
