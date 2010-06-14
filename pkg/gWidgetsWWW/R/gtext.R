
## no wrap argument in Ext
## font.attr not implemented -- use markup
## svalue
## svalue<-
## add (one line at end only)
## handlers?
gtext <- function(text = NULL, width = NULL, height = 300,
                  font.attr = NULL, wrap = TRUE,
                  handler = NULL, action = NULL, container = NULL,...,
                  resizable = FALSE     # gWidgetsWWW arg. Keep?
                  ) {

  if(!resizable) {
    widget <- EXTComponentText$new(toplevel=container$toplevel,
                               ..width = width,..height=height,
                               ..wrap = wrap)
    class(widget) <- c("gText", class(widget))
  } else {
    widget <- EXTComponentResizable$new(toplevel=container$toplevel,
                                        ..width = width,..height=height,
                                        ..wrap = wrap)
    class(widget) <- c("gText","gWidgetResizable", class(widget))
    
  }
  widget$setValue(value=text)
  

  ## CSS

  ## Scripts

  ## methods
  widget$add <- function(.,child, where="end", ...) {
    ## warp around svalue<-
    if(where == "end")
      svalue(.) <- c(svalue(.), child)
    else
      svalue(.) <- c(child,svalue(.))
##     curValue <- String(svalue(.))
##     for(i in child) {
##       curValue <- curValue + i
##     }
##     svalue(.) <- curValue
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
      theData <- paste(.$..data, collapse="\\n")
    } else {
      theData <- paste(.$..data, collapse="\\n")
    }
    out <- String() +
      .$asCharacter() + '.setValue(' +
        shQuote(theData) + ');' + '\n'

    return(out)

  }

  widget$transportSignal <- "change"
  widget$transportValue <- function(.,...) {
    out <- String() +
      'var value = escape(' + .$asCharacter() + '.' +
        .$getValueJSMethod + '());' + '\n'
    return(out)
  }
  widget$ExtConstructor <- "Ext.form.TextArea"
  widget$ExtCfgOptions <- function(.) {
    out <- list()
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
    out[["selectOnFocus"]] <- TRUE
    out[["enableKeyEvents"]] <- TRUE
    return(out)
  }

  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

