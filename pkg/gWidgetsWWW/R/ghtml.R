## Show marked up text -- or show url
## svalue<- only works for urls, not for text
## pass object of S3 class URL if want url and not absolute  (eg. http:///)


ghtml <- function(x, container = NULL, ...) {
  ## x is a url or a character vector to show
  ## components

  
  widget <- EXTComponent$new(toplevel=container$toplevel)
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
      out[['html']] <- paste(escapeQuotes(gsub("'","&#146;",svalue(.))), collapse="\\\\n")
    
    return(out)
  }
  
  
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
}
