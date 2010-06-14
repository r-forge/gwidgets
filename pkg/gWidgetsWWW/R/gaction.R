## make an action
## want to be able to use as
## a) menu/tool item as in Ext
## b) as action= argument to addHandler
## c) the method svalue<- should update instances of action (in Ext)
## d) the enabled<- should set for all instances
## XXX Actions *must* be added prior to showing the window (integrate into gsubwindow otherwise)


EXTAction = EXTComponent$new()

gaction <- function(label, tooltip=label, icon=NULL, handler, parent, ...) {

  ## parent should be a toplevel container
  if(inherits(parent,"gSubwindow"))
    window <- parent
  else
    window <- parent$toplevel
  
  ## make a class
  widget = EXTAction$new(label=label,tooltip=tooltip, icon=icon, handler=handler)
  class(widget) <- c("gAction",class(widget))
  widget$setValue(value = label)
  widget$window <- window
  
  ## get ID without adding to container
  widget$ID <- window$newID()
  
  window$addAction(widget)              # for printing
  widget$handlerID <- window$addHandler(signal = NULL, handler = handler)
  
  ## make Show method -- will call show when added as a handler?
  widget$no.x.hidden <- TRUE
  widget$ExtConstructor <- "Ext.Action"
  widget$ExtCfgOptions <- function(.) {
    handlerString <- String() +
      'function() {runHandlerJS(' +
        .$handlerID + ',\'""\',\'""\')}'

    out <- list(id = NULL,
                renderTo = NULL,
                text = svalue(.),
                handler = handlerString
                )
    if(exists("icon",envir=., inherits=FALSE))
      out[['iconCls']] <- .$icon

    return(out)
  }

  ## main methods are
  ## svalue to set text
  ## enabled to en/dis-able
  widget$setValueJS <- function(.,...) {
    out <- String() +
      .$asCharacter() + '.setText(' + shQuoteEsc(svalue(.)) + ');'
    return(out)
  }

  widget$setEnabledJS <- function(.,...) {
    out <- String() +
      .$asCharacter() + '.setDisabled(' + tolower(as.character(!.$..enabled)) + ');'
    return(out)
  }

  ## no S3 method for these
  widget$extSetIconClassJS <- function(.,icon,...) {
    out <- String() +
      .$asCharacter() + '.setIconClass(' + svalue(.) + ');'
    return(out)
  }

  widget$extSetHandlerJS <- function(.,handler,...) {
    .$handlerID <- .$window$addHandler(signal = NULL, handler = handler)
    handlerString <- String() +
      'function() {runHandlerJS(' +
        .$handlerID + ',\'""\',\'""\')}'
    out <- String() +
      .$asCharacter() + '.setHandler(' + handlerString + ');'
    return(out)
    
  }


  return(widget)


}


## implemented setIconClass, setHandler as in ext but no S3 methods here
