## gradio
## XXX no [<- method!!!
## size?,
## XXX transport is off the ranch -- is this necessary?


gradio <- function(items, selected = 1, horizontal=FALSE,
       handler = NULL, action = NULL, container = NULL, ...) {

  ## use a checkbox if only one item
  if(length(items) == 1) {
    out <- gcheckbox(items, checked = selected == 1, handler=handler, action=action, container = container, ...)
    return(out)
  }

  widget <- EXTComponentWithItems$new(toplevel=container$toplevel,
                                      ..selected = selected,
                                      ..horizontal = horizontal,
                                      ..handler = handler,
                                      ..action = action
                                      )
  class(widget) <- c("gRadio",class(widget))
  
  widget$setValue(value = selected) ## store the index
  widget$setValues(value = items)

  ## define methods
  ## we store values by index
  widget$getValue <- function(.,index=NULL ,drop=NULL,...) {
    ## we need to revers logic from AWidgtet$getValue
    out <- .$..data
    if(exists("..shown",envir=.$toplevel,inherits=FALSE)) {
      ## get from widget ID
      out <- try(get(.$ID,envir=.$toplevel),silent=TRUE) ## XXX work in index here?
      if(!inherits(out,"try-error")) {
        out <- as.numeric(out)          # is character
      } else {
        out <- .$..data
      }
    }
    ## no index -- return values
    if(is.null(index)) index <- FALSE
    if(index)
      return(as.numeric(out))
    else
      return(.$..values[as.numeric(out)])
  }
  
  ## override setValue
  widget$setValue <- function(., index=NULL,..., value) {
    ## values can be set by index or character
    if(is.null(index) || !index) {
      ind <- which(value %in% .$getValues())
      if(length(ind) == 0) return()
      ind <- ind[1]
    } else {
      ind <- value
    }
     
    ## we store the index
    .$..data <- ind

    if(exists("..shown",envir=., inherits=FALSE))
      cat(.$setValueJS(index=ind), file=stdout())
  }

  widget$setValueJS <- function(.,..., index) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    out <- String() +
      .$asCharacter() + '.getComponent(' + as.character((index-1)) +
        ')' + '.setValue(true);'
    return(out)
  }
  
  ## to set values we a) remove old values b) add new ones c) handlers?
  ## XXX doesn't work!!!
##   widget$setValuesJS <- function(.) {
##     out <- String()

##     ## JS to remove values
##     out <- out +
##       'var n = ' + 'o' + .$ID + '.items.getCount();' +
##         'var i = 0;' +
##           'while(i < n) {' +
##             'var rb = ' + 'o' + .$ID + '.getComponent(n - i - 1);' +
##               'o' + .$ID + '.remove(rb);' +
##                 'i = i + 1;' +
##                   ' };' + '\n'

##     ## JS to add new ones
##     out <- out + .$makeRadioButtons()
##     out <- out + .$addRadioButtons()
##     out <- out + .$addRadioButtonHandlers()
##     return(out)
##   }

  widget$xtype <- "radio"
  widget$transportSignal <- "check"
  widget$checked <- function(.,i) (i == .$..selected)
  widget$ExtCfgOptions <- function(.) {
    out <- list(border = FALSE,
                hideBorders = TRUE,
                shim = FALSE,
                bodyStyle = list(padding = "5px"),
                items = .$makeItems()
                )
    if(.$..horizontal)
      out[['layout']] <- "column"
    return(out)
  }

  ## transport
  widget$transportValue <- function(.,...,i) {
    out <- String() +
      'if(checked === true) {' +
        '_transportToR(' + shQuote(.$ID) +
          ',' +
            'Ext.util.JSON.encode({value:' + i + '})' +
            ');}' + '\n'         # i passed into transportValue()!

    return(out)
  }

  widget$transportFUN <- function(.) return(String(""))
  ## override to put with checked===true
  widget$writeHandlerFunction <- function(., signal, handler) {
    out <- String() +
          'if(checked === true) {' +
            'runHandlerJS(' + handler$handlerID
    if(!is.null(handler$handlerExtraParameters))
      out <- out + ',' + handler$handlerExtraParameters
    out <- out + ');' + 
      '};' + '\n'
    return(out)
  }

  ## add after CSS, scripts defined
  container$add(widget,...)


  ## add Handler
##   widget$addRadioButtonHandlers <- function(.) {
##     out <- String()
##     values <- .$getValues(); n <- length(values)
##     for(i in 1:n) {
##       out <- out +
##         .$ID + 'radiobutton' + i +
##           '.on("check",function(e,check) {' +
##             'if(check === true) {' +
##               ## do transport
##               '_transportToR(' + shQuote(.$ID) +
##                 ',' + i + ');' + '\n'
##       if(!is.null(.$..handler)) {
##         out <- out +
##           'runHandlerJS(' + .$..handlerID + ',\'""\', \'""\', true,this,{delay:100,buffer:100, single:false});'
##       }
##       out <- out +'}});' + '\n'
##     }
##     ## we need to add this handler for *each* radio button
##     ## we add transport to R and handler if present.
##     return(out)
##   }

##   widget$addHandler <- function(.,signal, handler, action=NULL,...) {
##     id <- get("addHandler",envir=EXTWidget, inherits=FALSE)(.,
##                                               signal=NULL, handler,action,...)
##     .$..handlerID <- id
##     invisible(id)
##   }

  widget$addHandlerClicked <- function(., handler, action=NULL, ...) 
    .$addHandler(signal="check", handler, action=NULL, ...)
  

  ## we add handler regardless, as this introduces transport function
  if(is.null(handler))
    signal <- NULL
  else
    signal <- "check"
  id <- widget$addHandler(signal=signal, handler, action)
  invisible(widget)

}
