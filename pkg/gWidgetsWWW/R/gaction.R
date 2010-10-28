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
  widget <- EXTAction$new(label=label,tooltip=tooltip, icon=icon, handler=handler)
  class(widget) <- c("gAction",class(widget))

  widget$toplevel <- parent$toplevel
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
    out <- sprintf("%s.setText(%s)", .$asCharacter(), shQuoteEsc(svalue(.)))
    ## out <- String() +
    ##   .$asCharacter() + '.setText(' + shQuoteEsc(svalue(.)) + ');'
    return(out)
  }

  widget$setEnabledJS <- function(.,...) {
    val <-  tolower(as.character(!.$..enabled))
    out <- sprintf("%s.setDisabled(%s)", .$asCharacter(), val)
    ## out <- String() +
    ##   .$asCharacter() + '.setDisabled(' + tolower(as.character(!.$..enabled)) + ');'
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
