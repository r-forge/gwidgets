## BROKEN -- no tansport. Can get "on" method for widget to work like other Ext things.
## no handler
## transport needs work
gcodemirror <- function(text = NULL, width = NULL, height = 300,
                        container = NULL,...,
                        codeMirrorPath="/codemirror"
                        ) {

  widget <- EXTComponentText$new(toplevel=container$toplevel,
                                 codeMirrorPath=codeMirrorPath,
                                 ..width = width,..height=height)
  class(widget) <- c("gText", class(widget))
  
  widget$setValue(value=text)
  

  ## CSS

  ## Scripts
  widget$scripts <- function(.) {
    f <- system.file("javascript","ext.ux.form.CodeMirror.js", package="gWidgetsWWW")
    out <- paste(readLines(f, warn=FALSE), collapse="\n")
    
    return(out)
  }

  
  
  ## methods
  widget$add <- function(.,child, where="end", ...) {
    ## warp around svalue<-
    if(where == "end")
      svalue(.) <- c(svalue(.), child)
    else
      svalue(.) <- c(child,svalue(.))
  }
  widget$insert <- widget$add
  
  ## methods
  ## getValue must escape the strings -- they are URL encoded by escape()
  widget$coerce.with <- function(., val) unescapeURL(val)
  
  
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"

  ## lots of escapes for multiline stuff
  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

    if(gWidgetsWWWIsLocal()) {
      theData <- paste(.$..data, collapse="\\\\n")
    } else {
      theData <- paste(.$..data, collapse="\\n")
    }
    out <- String() +
      .$asCharacter() + '.setValue(' +
        shQuote(theData) + ');' + '\n'

    return(out)

  }
  ## transport
  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = escape(' + .$asCharacter() + '.' +
        .$getValueJSMethod + '());' + '\n'
    return(out)
  }
  ## constructor
  widget$ExtConstructor <- "Ext.ux.form.CodeMirror"
  widget$ExtCfgOptions <- function(.) {
    out <- list(language="js"
                ,codeMirrorPath=.$codeMirrorPath
                ,listeners=String("{'change': {scope: this, fn: function(field, newValue, oldValue) {alert('hi');}}}")
                )
    
    out[["value"]] = paste(svalue(.), collapse="\\\\n")
    if(!is.null(.$..width)) {
      out[["width"]] <- .$..width
    } else {
      out[["width"]] <-  "auto"
    }
    if(!is.null(.$..height)) {
       out[["height"]] <- .$..height
     } else {
       out[["height"]] <-  "auto"
     }
    out[["enableKeyEvents"]] <- TRUE
    return(out)
  }

  ## add after CSS, scripts defined
  container$add(widget,...)


  invisible(widget)
}

