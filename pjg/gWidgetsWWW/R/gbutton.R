## button -
## methods
## svalue works
## svalue<- works
## addHandlerClicked works

## *IF* handler = NULL and action a gaction instance then
## will use that action. Not for addHandlerClicked though.

gbutton <- function(text="", border=TRUE,
                    handler = NULL, action=NULL, container, ...) {
  ## components
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..handler = handler,
                             ..action=action
                             )
  class(widget) <- c("gButton",class(widget))
  widget$setValue(value=text)

  ## function to check is we have a gaction object
  widget$doAction <- function(.) {
    if(!exists("..handler", envir=., inherits=FALSE) &&
       exists("..action", envir=., inherits=FALSE) &&
       !is.null(.$..action) &&
       inherits(.$..action,"gAction")
       ) return(TRUE)
    if(is.null(.$..handler) &&
       exists("..action", envir=., inherits=FALSE) &&
       !is.null(.$..action) &&
       inherits(.$..action,"gAction")
       ) return(TRUE)
    return(FALSE)
  }
  ## properties
  widget$getValueJSMethod <- "getText"
  widget$setValueJSMethod <- "setText"
  widget$transportSignal <- NULL        # no transport
  widget$ExtConstructor <- "Ext.Button"
  widget$ExtCfgOptions <- function(.) {
    out <- list("text" = svalue(.))
    if(.$doAction())
      out[['text']] <- svalue(.$..action)
    ## add an icon
    text <- svalue(.)
    si <- getStockIcons()
    if(!is.na(si[text])) {
      out[['cls']] <- "x-btn-text-icon"
      out[['icon']] <- si[text]
    }

    return(out)
  }

  ## intercept action possibility
  widget$writeConstructor <- function(.) {
    
    if(.$doAction()) {
      out <- String() +
        'o' + .$ID + '= new ' + .$ExtConstructor + '(' +
          .$..action$asCharacter() + ');'
      out <- out +
        .$asCharacter() + '.id=' + shQuote(.$ID) + ';' 
      out <- out +
        .$asCharacter() + '.render(document.body);' + '\n'
    } else {
      out <- get("writeConstructor",envir=EXTWidget)(.)
    }
    return(out)
  }

  
  ## add after CSS, scripts defined
  container$add(widget,...)

  if(!is.null(handler))
    widget$addHandlerClicked(handler=handler,action=action)
  
  invisible(widget)
}
  
