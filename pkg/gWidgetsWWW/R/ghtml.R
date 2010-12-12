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

## Show marked up text -- or show url
## svalue<- only works for urls, not for text
## pass object of S3 class URL if want url and not absolute  (eg. http:///)

##' widget to render HTML text pages
##'
##' 
##' @param x an HTML string or a URL. (However, URL's are not working!)
##' \code{svalue<-}. If an HTML fragment, then assumed to be HTML. THe
##' \code{svalue<-} method has an extra argument \code{encode}, which
##' if \code{TRUE} will encode the HTML bits. This is \code{FALSE} by
##' default.
##' @param container parent container
##' @param ... passed to add method of parent container
##' @export
ghtml <- function(x, container = NULL,  ...) {
  ## x is a url or a character vector to show
  ## components

  
  widget <- EXTComponentNoItems$new(toplevel=container$toplevel)
  class(widget) <- c("gHtml",class(widget))
  widget$setValue(value=x)

  ## helper function
  widget$htmlEscape <- function(., val) {
    val <- gsub("\n","<br />", val)
    val <- gsub("'", "&#146;", val)   # get rid of ' issue
    val <- escapeQuotes(val)
    val
  }


  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    val <- .$..data
    out <- String() + 'o' + .$ID
    if(isURL(val)) {
      out <- sprintf("%s.load(%s); %s.update();", .$asCharacter(), ourQuote(val), .$asCharacter())
    } else {
      ## this depends on local or non-local
      ## if(gWidgetsWWWIsLocal()) {
      ##   val <- paste(val, collapse="\\\\n")
      ## } else {
      ##   val <- paste(val, collapse="\\n")
      ## }

      ## do we encode? By default false
      doEncode <- ifelse(getFromDots(..., var="encode", default=FALSE), "true", "false")

      ## was stripSlashN bit
      out <- sprintf("%s.setText('%s', %s);", .$asCharacter(), paste(.$htmlEscape(svalue(.)), collapse=" "), doEncode)
    }
    return(out)
  }

  if(isURL(x)) 
    widget$ExtConstructor <- "Ext.Panel"
  else
    widget$ExtConstructor <- "Ext.form.Label"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    
    if(isURL(svalue(.)))
      out[['autoLoad']] <- list(url=svalue(.))
    else
      out[['html']] <- paste(.$htmlEscape(svalue(.)), collapse=" ") # was \\\\n but gives issues locally
    
    return(out)
  }
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
}
