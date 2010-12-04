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


ghtml <- function(x, container = NULL, ...) {
  ## x is a url or a character vector to show
  ## components

  
  widget <- EXTComponentNoItems$new(toplevel=container$toplevel)
  class(widget) <- c("gHtml",class(widget))
  widget$setValue(value=x)

  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    val <- .$..data
    out <- String() + 'o' + .$ID
    if(isURL(val)) {
      out <- out +
        '.load(' + shQuote(val) + ');'
    } else {
      ## this depends on local or non-local
      if(gWidgetsWWWIsLocal()) {
        val <- paste(val, collapse="\\\\n")
      } else {
        val <- paste(val, collapse="\\n")
      }

      val <- gsub("'", "&#146;", val)   # get rid of ' issue
      val <- escapeQuotes(val)
      out <- out +
        '.setText(' + shQuoteEsc(val) + ', false);'
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
      out[['autoLoad']] <- svalue(.)
    else
      out[['html']] <- paste(escapeQuotes(gsub("'","&#146;",svalue(.))), collapse=" ") # was \\\\n but gives issues locally
    
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
}
