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

require(proto, quietly=TRUE)

## Three main classes
## EXTWidget
##   EXTComponent
##   EXTContainer

## The double dot is meant to indicate an instance variable/method as
## opposed to a "class" variable/method. It seems that proto does not
## propogate the double dot, so these are set in the widget constructor (as
## they should be

## > a = proto(..test = TRUE, test = TRUE, new = function(.) .$proto())
## > b = a$new()
## > b$test
## [1] TRUE
## > b$..test
## Error in get(x, env = this, inherits = inh) : 
##   variable "..test" was not found
##
## so we check for ..test using exists and inherits = FALSE

##' Base trait for all widget/components/stores etc
EXTWidget <-
  proto(new = function(.,...) {
    obj <- .$proto(...)
    class(obj) <- c("gWidget",class(obj))
    return(obj)
  },
        ## properties
        toplevel = NULL,                # stores top gwindow instance
        parent = NULL,                  # stores parent (sub)window
        ID = "",                        # what am i, IDXXX
        .tag=list(),                    # for tag<-
        ..data = NULL,                  # for svalue
        ..values = NULL,                # for [
        ..enabled = TRUE,               # is enabled (or grayed out)
        ..visible = TRUE,               # logical, false if don't show
        ..shown = FALSE,                # is this object shown (rendered)?
        css = NULL,
        scripts = NULL,                 # javascript stuff per widget
        style = c(),                    # style options
        ExtConstructor = NULL,          # eg. Ext.Button
        ExtCfgOptions = NULL,           # config options -- fn returns a list
        ..ExtCfgOptions = NULL,         # additional options per instance
        getValueJSMethod = NULL,        # name of method, eg. "getValue"
        setValueJSMethod = NULL,        # name of method, eg. "setValue"
        coerce.with = NULL,             # coerce FUN or string
        transportSignal = NULL          # possibly a vector
        )


### methods ##################################################
##' has a slot? mapping of exists. From mutatr
##' 
##' @param key name of slot
##' @return logical
EXTWidget$has_slot <- function(., key) exists(key, envir=.)

##' is slot local to object (not inherited)
##' 
##' @param key name of slot
##' @return logical
EXTWidget$has_local_slot <- function(., key) exists(key, envir=., inherits=FALSE)

## or puts onto JS queue
##' @param ... pasted together to form a string
##' @param queue to we push onto JSQueue or print out. (Pushing is used if returning JS)
EXTWidget$Cat <- function(.,..., queue=FALSE) {
  out <- paste(..., collapse="\n")
  if(queue)
    .$addJSQueue(out)
  else
    cat(out)
}

## Cat either a string or function
##' Helper to cat out part of webpge
##' @param part name of part or function defining part
##' @param queue passed to Cat method of .
##' @return will print out page or queue it
EXTWidget$showPart <- function(.,part, queue=FALSE) {
  ## part may be property of function. This checks
  if(!is.null(part))
    if(is.function(part))
      .$Cat(part(), queue=queue)
    else
      .$Cat(part, queue=queue)
}

## instead of pasting "o" in front of ID
## we have Ext Object "o" + ID and DOM object "ID" to consider
## XXX This is misnamed (toString?)

##' Represent object as character, similar to tcltk ID
##'
##' The character is usually the name of the object in EXT.
##' @return a character
EXTWidget$asCharacter <- function(.) {String('o') +  .$ID}

##' Some widgets are found within a panel. This allows one to override
EXTWidget$asCharacterPanelName <- function(.) .$asCharacter()

##' simple function to call an Ext method on the corresponding object
##'
##' @param methodname name of method
##' @param args arguments -- a string! such as "e,w"
##' @return a string, JS, to call method
EXTWidget$callExtMethod <- function(., methodname, args) {
  if(missing(args))
    args <- ""
  out <- sprintf("%s.%s(%s);\n", .$asCharacter(), methodname, args)
  return(out)
}

##' Is this a toplevel widget?
##'
##' @param . self
##' @return logical
EXTWidget$isToplevel <- function(.) .$identical(.$toplevel)


## We have both S3 methods and their proto counterparts

## Here we have getValue, setValue for svalue, svalue<-
## svalue, svalue<-

##' EXT widget for svalue
##'
##' @param index see svalue
##' @param drop see svalue
EXTWidget$getValue <- function(., index=NULL,drop=NULL, ...) {
  ## if(exists("..shown",envir=.,inherits=FALSE)) {
     ## ## get from widget ID
     ## out <- try(get(.$ID,envir=.$toplevel),silent=TRUE) ## XXX work in index here?
     ## if(inherits(out,"try-error")) {
     ##   out <- .$..data
     ## } else {
     ##   .$..data <- out                  # update data
     ## }
  ## } else {

  out <- .$..data
  ## if(is.null(index) || !index) {
  ##   out <- .$..data
  ## } else {
  ##   values <- .$getValues()
  ##   if(is.data.frame(values))
  ##     values <- values[,1, drop=TRUE]
  ##   out <- which(.$..data %in% values)
  ## }
  ##}
  out <- .$coerceValues(out)
  return(out)
}

##' Set the widget value
##'
##' have we shown the widget? if so, we set in document too
##' We need to also assign to .$ID in . as otherwise
##' we don't get the getValue right
##' @param index see \code{svalue<-}
##' @param value what will be set
##' @return sets value quietly, Adds to JS queue if apt
EXTWidget$setValue <- function(., index=NULL, ..., value) {
  ## override locally if desired
  if(exists("..setValue",envir=., inherits=FALSE)) {
    .$..setValue(index=index, ..., value=value)
  } else {
    
    ## store index
    if(!is.null(index)) {
      items <- .$getValues();  d <- dim(items)
      if(is.null(d) || d == 1)
        newVal <- items[value,1]
      else
        newVal <- items[value,]
    } else {
      newVal <- value
    }
    .$..data <- newVal
  }
  ## now process if shown
  if(exists("..shown",envir=., inherits=FALSE)) 
    .$addJSQueue(.$setValueJS(index=index, ...))

 }
## create javascript code to write Javascript to set
## the value of a widget
## properties setValueMethod,

##' property for where to set javascript when outside the Ext methods/properties
EXTWidget$setValueJSAttribute = "innerHTML"



##' Method call to create JavaScript to set a value
##'
EXTWidget$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
  
  ## default method to set the value using setValue
  value <- .$..data                     # don't use svalue here
  
  ##' this uses the DOM value -- not the Ext widget. EXTComponent
  ##' overrides this.
  out <- sprintf("var widget= EXT.get(%s).%s = %s;", shQuote(.$ID), .$setValueJSAttribute, shQuote(value))
  return(out)                            
}


##' method to coerce values using either coerce.with or ..coerce.with
##'
##' @param values can override or take \code{..values} from object
EXTWidget$coerceValues <- function(.,values = .$..values) {
  coerce.with = NULL
  if(exists("..coerce.with", envir=., inherits=FALSE))
    coerce.with <- .$..coerce.with
  else if (exists("coerce.with", envir=., inherits=TRUE))
    coerce.with <- .$coerce.with
  
  if(is.null(coerce.with)) return(values)

  if(is.character(coerce.with))
    return(do.call(coerce.with, list(values)))
  else
    return(coerce.with(values))
}
  


## getValues = [; setValues = [<-
## [, [<-

##' method for \code{[}
EXTWidget$getValues <- function(., ...) .$..values

##' method for \code{[<-}
##' @param i ignored -- XXX FIX THIS
##' @param j ignored -- XXX fix this
##' @param ... passed to setValuesJS
##' @param value value to set
##' @return adds to JS queue if shown
EXTWidget$setValues <- function(.,i,j,...,value) {
  ## XXX Need to include i,j!
  .$..values <- value
  if(.$has_local_slot("..shown"))
    .$addJSQueue(.$setValuesJS(...))
}

##' call to set values via JS
##' @param ... passed to local function
EXTWidget$setValuesJS <- function(.,...) {
  if(.$has_local_slot("..setValuesJS"))
     .$addJSQueue(.$..setValuesJS(...))
}

## length, dim -- issue with recursion if given full name

##' length method for gWidget class
##' 
##' @param x gWidgets instance.
length.gWidget <- function(x) {. = x; .$.length()}

##' EXT length method
##'
##' @return length of object
EXTWidget$.length <- function(.) {vals <- .$..values; length(vals)}

##' dim method for gWidget
##' @param x gWidget object
dim.gWidget <- function(x) {. = x; .$.dim()}

##' EXT dim method
EXTWidget$.dim <- function(.) {vals <- .$..values; dim(vals)}

## names, names<-
EXTWidget$getNames <- function(.) .$..names
EXTWidget$setNames <- function(.,value) {
  .$..names <- value
  if(exists("..shown",envir=., inherits=FALSE)) {
    .$addJSQueue(.$setNamesJS())
  }
}
##' Method to set names attribute of object in Javascript
EXTWidget$setNamesJS <- function(.) {}    # set names


##' Method for visible()
EXTWidget$getVisible <- function(.) return(.$..visible )

##' method to set visibility visible<-
##'
##' @param value logical
EXTWidget$setVisible <- function(.,value) {
  .$..visible <- as.logical(value)
  if(exists("..shown",envir=., inherits=FALSE)) {
    .$addJSQueue(.$setVisibleJS())
  }
}

##' javascript to synchronize widget with R value
EXTWidget$setVisibleJS <- function(.) {
  if(exists("..setVisibleJS", envir=., inherits=FALSE))
    .$..setVisibleJS()
  
  value <- .$..visible
  if(as.logical(value))
    action = "show"
  else
    action = "hide"
  .$callExtMethod(action)
}

## enabled<-

##' is widget enabled
##' 
##' @return logical
EXTWidget$getEnabled <- function(.) return(.$..enabled )

##' method to set if widget is enabled
##' @param value logical
EXTWidget$setEnabled <- function(.,value) {
  .$..enabled <- as.logical(value)
  if(.$has_local_slot("..shown"))
    .$addJSQueue(.$setEnabledJS())
}
##' method to write javascript to synchronize widget with R value
EXTWidget$setEnabledJS <- function(.) {
  if(exists("..enabled", envir=., inherits=FALSE))
    value <- as.logical(.$..enabled)
  else
    value <- TRUE

  ## which method
  out <- sprintf("%s.%s()\n", .$asCharacter(), ifelse(value, "enable", "disable"))
  return(out)
}


## ..style covers fonts, size, and others
## font uses this
EXTWidget$setFont <- function(.,value) {
  
}
## XXX integrate with setStylesJS
EXTWidget$setStyleJS <- function(.,styles = NULL) {
  ## styles
  if(is.null(styles)) {
    styles <- .$style
  }
  if(exists("..style",envir=., inherits=FALSE)) {
    for(i in names(.$..style))
      styles[i] <- .$..style[i]
  }
  
  if(length(styles) > 0) {
    out <- String() 
    
    for(i in names(styles)) {
      out <- out +
        'Ext.get(' + shQuote(.$ID) + ').setStyle(' + shQuote(i) + ',' +
          coerceToJSString(styles[i]) + ');\n'
    }
  } else {
    out <- String()
  }
  return(out)
}
  

##' set size fo widget
##' 
##' calls setStyle
##' @param value a vector c(width), c(width, height)
EXTWidget$setSize <- function(., value) {
  ## fix size in ..style
  if(exists("..style",envir=., inherits=FALSE))
    curStyle <- .$..style
  else
    curStyle <- c()
  
  n <- length(value)
  if(n == 0) return()
  
  curStyle["width"] <- .$..width <- value[1]
  
  
  if(n > 1) {
    curStyle["height"] <- .$..height <- value[2]
  }
  
  .$..style <- curStyle
  return()
}

##' method to return size (in pixels) of widget
##'
##' @return integer vector with two components
EXTWidget$getSize <- function(.) {
  if(!exists("..style",envir=., inherits=FALSE))
    return(c(width=NULL,height=NULL))
  
  curStyle <- .$..style
  return(c(width=curStyle$width,height=curStyle$height))
}


## Methods to print out the javascript to create a widget
## using Ext. There are several stages that can be overridden. For
## example, see EXTComponentWithStore where writeConstructor is overridden.

## these options are inherited by others. Can be overridden
## by the widget
## Standard Configuration options
## also have ExtCfgOptions for each widget (class based)
## and ..ExtCfgOptions for instances

## The function mapRtoObjectLiteral pieces together the list.
## The one subtlety is that characters get quoted, String()'s do not.
## the function is recursive, as some options may be given as
## object literals in Ext

##' Create a list of standard configuration options
##'
##' Can be overridden, subclasses, ...
##' @return a list with the options. List is passed to mapRtoObjectLiteral
EXTWidget$ExtStdCfgOptions <- function(.) {
  out <- list(
              "id"=.$ID
              )
  out[["renderTo"]] <- String(.$toplevel$..renderTo) #String("Ext.getBody()"),

  if(exists("..enabled",envir=., inherits = FALSE))
    if(!.$..enabled)
      out[['disabled']] <- !.$..enabled   # in Ext.Component
  if(exists("..visible",envir=., inherits = FALSE))
    if(!.$..visible)
      out[['hidden']] <- !.$..visible   # in Ext.Component
  if(exists("..tpl", envir=., inherits=FALSE)) {
    out[['tpl']] <- .$..tpl()
  } else if(exists("tpl", envir=., inherits =FALSE)) {
    out[['tpl']] <- .$tpl()
  }
  
  ## XXX how to integrate styles into this?
  return(out)
}


       
##' method to coerce ExtCfgOptions into a string
##' 
##' ExtCfgOptions is a list. The names are passed as keys and the values
##' are assigned.
##' Object Literals in Ext are like passing lists through the ... argument
##' characters are quoted, String()s are not. Sometimes
##' as.character(String() + "...") is useful.
##' This function recurses when the value is a list
##' method coerceToJSString is in common.R
##' @param values value to be mapped
##' @param doBraces if TRUE will wrap in {}
##' @return javascript string 
EXTWidget$mapRtoObjectLiteral <- function(.,values,doBraces=TRUE) {
  
  if(missing(values)) {
    ## pull in from std, class configuration, instance values
    values <- .$ExtStdCfgOptions()

    if(exists("ExtCfgOptions", envir=., inherits=TRUE) &&
       !is.null(.$ExtCfgOptions)) {

      cfgValues <- .$ExtCfgOptions()
      for(i in names(cfgValues))
        values[[i]] <- cfgValues[[i]]
    }
    ## add to values if some there
    if(exists("..ExtCfgOptions", envir=., inherits=FALSE)) {
      instanceValues <- .$..ExtCfgOptions()
      for(i in names(instanceValues)) {
        if(!is.null(instanceValues[[i]]))
          values[[i]] <- instanceValues[[i]]
      }
    }
  }
  

  ## values is a list, we need to make a vector of strings
  out <- c()
  for(i in names(values)) {
    if(!is.null(values[[i]])) {
      ## recurse if a list
      if(is.list(values[[i]])) {
        out[i] <- .$mapRtoObjectLiteral(values[[i]], doBraces=TRUE)
      } else {
        out[i] <- coerceToJSString(values[[i]])
      }
    }
  }

  res <- paste(names(out), out, sep=":", collapse=",\n\t")
  if(doBraces)
    res <- String('{\n\t') + res + '\n}'

  return(res)
}

## Basic template for a EXT widget.
## There are several stages.
## header and footer are used to wrap the object if desired

##' header for a widget.
##'
##' Meant to be overridden in subclass
##' @return returns text to be placed in the header
EXTWidget$header <- function(.) return("")

##' footer for the widget
##'
##' Meant to be overridden in subclass
##' @return returns text to be placed in the header
EXTWidget$footer <- function(.) return("")

##' widget separator
EXTWidget$separator <- function(.) return("")

##' Method to write out the constructor
##'
##' Assumes property \code{ExtConstructor} is set
##' @return string container JS code for constructor
EXTWidget$writeConstructor <- function(.) {
  out <- String() + "\n" +
### var creates a *local* variable -- issues with safari here
###    'var o' + .$ID +
    .$asCharacterPanelName() +
      ##'o' + .$ID +
      ' = new ' +.$ExtConstructor + '(' +
        .$mapRtoObjectLiteral() +
          ');\n'

  ## write out x-hidden unless requested not to.
  ## x-hidden causes the widget not to display until added to parent
  if(!.$has_local_slot("..shown") && (.$has_local_slot("x.hidden") && .$x.hidden))
    out <- out +
      sprintf("Ext.get('%s').addClass('x-hidden');\n", .$ID)

  
  ## add in at the end 
  if(exists("..writeConstructor", envir=., inherits=FALSE)) {
    out <- out + .$..writeConstructor() + "\n"
  }

  return(out)
}

## For controls whose value may be changed by the GUI, we write out changes
## immediately back to R so that R handlers will be aware of the changes. We
## call this transport. The method assignValue is used within R to assign these values into the widget
## The basic call involves  Ext.util.JSON.encode({value:value}) on one end
## and this decode by fromJSON on the other end.

##' code to write out value definition of transport function
##' 
##' called in writeHandlers
##' @param ... ignored
##' @return javascript string
EXTWidget$transportValue <- function(.,...) {
  out <- sprintf("var value = %s.%s();\n", .$asCharacter(), .$getValueJSMethod)
  return(out)
}

##' write out transport function
##'
##' @return javascript string
EXTWidget$transportFUN <- function(.) {
  out <- sprintf("_transportToR(%s, Ext.util.JSON.encode({value:value}) );", shQuote(.$ID))
  return(out)
}

##' piece together transport string
##'
##' @return javascript string writing out body of transport function
EXTWidget$writeTransport <- function(.,ext="",signal=NULL) {
  ## transport to R
  if(!is.null(.$transportSignal)) {
    out <- String() +
      .$transportValue(i = ext,signal=signal) + # (For EXTComponentWithItems)
        .$transportFUN() +
          '\n'
  } else {
    out <- String("")
  }
  return(out)
}

### Tooltip
##' property for width of tooltip
EXTWidget$tooltipWidth <- 200
##' propert to hide tooltip after time out
EXTWidget$tooltipAutoHide <- TRUE # override to

##' method to write out tooltip padded with \code{tooltip<-} method.
##'
##' writes tooltip. Tooltips are added with tooltip<- function
##' value can be a URL (isURL == TRUE) or a string or a character vector which
##' gets pasted together to be a string
##' @return javascript string
EXTWidget$writeTooltip <- function(.) {
  out <- String()
  ## tooltip
  if(exists("..tooltip", envir=., inherits=FALSE)) {
    lst <- list(target=.$ID,
                showDelay=100,
                hideDelay=50,
                autoHide = .$tooltipAutoHide,
                trackMouse = TRUE,
                width = .$tooltipWidth)            # default size?

    if(isURL(.$..tooltip)) {
      lst[["autoLoad"]] <- String('{url:') + shQuote(.$..tooltip) + '}'
    } else {
      ## ..tooltip can be a) a string, b) a character vector of c) a list with components title and message
      if(is.list(.$..tooltip)) {
        lst[['title']] <- .$..tooltip$title
        message <- .$..tooltip$message
      } else {
        message <- .$..tooltip
      }
      lst[["html"]] <- paste(escapeQuotes(message), collapse="<BR>")
    }

    if(!.$tooltipAutoHide) {
      lst[["closable"]] <- TRUE
      lst[["draggable"]] <- TRUE
    }
    out <- out +
      'var tooltip' + .$ID + '= new Ext.ToolTip(' +
        +  .$mapRtoObjectLiteral(lst) + ');' + '\n'
  }
  return(out)
}

##' show object,
##' 
##' Called by show(). This method cat's out value
##' Called once while GUI is drawn, so catted out, not added to queue
EXTWidget$show <- function(., queue=FALSE) {
  out <- String("\n") +
    .$writeConstructor() +
      .$setStyleJS(styles=.$style) +
          .$writeTooltip() +
            .$writeHandlersJS()           # includes transport

  if(.$has_local_slot("..visible"))
    .$setVisibleJS()
  
  .$..shown <- TRUE
  .$Cat(out, queue=queue)
}

##' An init method
##'
##' Adds instance to toplevel list of children
##' @param . self
EXTWidget$init <- function(.) {
  if(.$has_local_slot("toplevel")) {
    .$toplevel$addChild(.)
  }
}

##' Assign value passed in from browser via transportToR
##'
##' Default is just svalue, but many other widgets require more than this
##' @param . self
##' @param value value to assign. May be a vector or list (from JSON conversion)
EXTWidget$assignValue <- function(., value) {
  .$..data <- value[[1]]
#  svalue(., index=TRUE) <- value[[1]]   # value is a list
}

##################################################
## Some "subclasses" of EXTWidget defined below

##' EXT Component -- for controls, etc. Main subclass
EXTComponent <- EXTWidget$new()

## public API. Call Show which wraps show withing header, tooltip,
## separators, footer,
## A widget will show
## header
## css (gwindow)
## scripts (gwindow)
## tooltip
## .$show()
## setHandlers (gwindow)
## footer

## unlike a Container, these have no children

##' Show method for cmponents
##'
##' Shows components, sets ..shown property
##' @param ... pass in queue=TRUE to queue up, otherwise cat's out
##' @return NULL
EXTComponent$Show <- function(.,...) {        # wraps Show
  ## add in any instance specific scripts
  ## component specific scripts written out once in gwindow
  if(exists("..scripts",envir=., inherits=FALSE)) 
    .$showPart(.$..scripts, ...)
  
  ## make an object with method?
  if(exists("..header",envir=.,inherits=FALSE))  .$showPart(.$..header, ...)
  .$showPart(.$header, ...)

  
  .$show(...)                   # show self
  
  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer, ...)
  .$showPart(.$footer, ...)

  .$..shown <- TRUE             # set shown (rendered)

}


##' 
### Methods have two parts
### * one for first showing (sets value in R)
### * one after shown -- returns string with javascript to synchronize R to browser

##' Property for setValue if assigning to a property
EXTComponent$setValueJSAttribute <- "value"
##' method call for setting values
EXTComponent$setValueJSMethod = "setValue"  # oID.method()

##' javascript to synchronize main value of component
##'
##' @return javascript string
EXTComponent$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
   ## default method to set the value using setValue
   value <- .$..data                     # don't use svalue here


   ## use Ext object and a method directly -- no DOM
   out <- String() +
     'o' + .$ID +'.' + .$setValueJSMethod +
       '(' + toJS(.$..data) + ');' + '\n'
   
   return(out)                              # to browser, not filewi
 }



### Different components ##################################################

##' A component without items (so can't set value the same way)
##'
##' Examples are buttons, statusbar, ... These don't have a \code{[} method or a getValues/setValues bit
EXTComponentNoItems <- EXTComponent$new()
EXTComponentNoItems$x.hidden <- FALSE

##' setValue for componets without items
##'
##' setValue just stuffs into \code{..data}
EXTComponentNoItems$setValue <- function(., index=NULL, ..., value) {
  ## override locally if desired
  if(exists("..setValue",envir=., inherits=FALSE)) {
    .$..setValue(index=index, ..., value=value)
  } else {
    .$..data <- value
  }
  ## now process if shown
  if(exists("..shown",envir=., inherits=FALSE)) 
    .$addJSQueue(.$setValueJS(index=index, ...))
}


##################################################
##' a resizable component
EXTComponentResizable <- EXTComponent$new()

## footer adds in a resizable conainer -- not working?
EXTComponentResizable$footer <- function(.) {
  lst <- list(id  = as.character(.$ID + 'resizer'),
              wrap = TRUE,
              pinned = TRUE)
  if(inherits(.,"gImage"))
    lst[['preserveRatio']] <- TRUE

  out <- String() +
    'new Ext.Resizable(' + shQuote(.$ID) + ',' +
      .$mapRtoObjectLiteral(lst) + ');\n'

  return(out)
  }

### Text component ##################################################
##
## We have gedit, gtext with key events, that are a bit different for handlers
## as we want to intercept the event with javascript

##' main trait for text components
EXTComponentText <- EXTComponent$new()

##' Assign value -- coerce to text
EXTComponentText$assignValue <- function(., value) {
  .$..data <- paste(value[[1]], collapse="\n")
}

##' method to write handler
##'
##' @return javascript string function(...) {...} NO ; at end
EXTComponentText$writeHandlerFunction <- function(., signal, handler) {
   out <- String()  +
     sprintf("function(%s) {runHandlerJS(%s%s);",
             .$handlerArguments(signal),
             handler$handlerID,
             ifelse(!is.null(handler$handlerExtraParameters),
                    paste(",", handler$handlerExtraParameters, sep=""),
                    "")
             )
     
   ## 'function(' + .$handlerArguments(signal) + ') {'
   ## tmp <- String() +
   ##   'runHandlerJS(' + handler$handlerID
   ## if(!is.null(handler$handlerExtraParameters)) {
   ##   tmp <- tmp + "," + handler$handlerExtraParameters
   ## }
   ## tmp <- tmp + ');'

   ## need to do transport
   ## tmp1 <- sprintf("var value = escape(%s.getValue());_transportToR(%s, Ext.util.JSON.encode({value:value}));",
   ##                 .$asCharacter(), shQuote(.$ID))


   if(!is.null(handler$args$key)) {

     keyMatch <- ""
     if(!is.null(key <- handler$args$key)) {
       keyMatch <- ifelse(is.numeric(key) || nchar(key) == 1, shQuote(key), key)
     } else if(!is.null(key <- handler$args$charCode)) {
       keyMatch <- ifelse(nchar(key) == 1, shQuote(key), key)
     }
     
     out <- out +
       paste(sprintf("if(e.getCharcode() == %s) {",keyMatch),
             sprintf("var value = escape(%s.getValue());", .$asCharacter()),
             sprintf("_transportToR('%s', EXT.util.JSON.encode({value:value}));", .$ID),
             "}",
           sep="\n")
   }

   out <- out + "}\n\n"
   
   ## ## wrap inside conditional
   ## if(!is.null(handler$args$key)) {
   ##   key <- handler$args$key
   ##   out <- out + "if(e.getCharCode() ==" +
   ##     ifelse(is.numeric(key) || nchar(key) == 1, shQuote(key),key) +
   ##     ") {" +
   ##       tmp1 +
   ##         tmp +
   ##         "};"
   ## } else if(!is.null(handler$args$charCode)) {
   ##   key <- handler$args$charCode
   ##   out <- out + "if(e.getCharCode() ==" +
   ##     ifelse( nchar(key) == 1, shQuote(key),key) + ") {" +
   ##       tmp1 + tmp +
   ##       "};"
   ## } else {
   ##   out <- out + tmp
   ## }
   ## ## close up
   ## out <- out + '}' + '\n\n'

   return(out)
}

### Container Trait.  ##################################################
## Main methods are:
## newID -- ID for a widget generated when added to a container.
##   This also copies over stuff to
## add -- adds to list for later rendering
## Show -- to show object -- loops over children

EXTContainer <- EXTWidget$new(children = list(),
##XXX                              width = "auto",
##                              height = "auto",
                              width=NULL, height=NULL, ## JV XXX
                              makeItemsFixedItems = "" # to make items[]
                              )
##' A new id
##'
##' Each child gets its own ID
##' @return a new id (gWidgetID##)
EXTContainer$newID <- function(.) {
  IDS <- .$toplevel$..IDS
  n <- length(IDS)
  newID <- sprintf("gWidgetID%s", n+1)
  .$toplevel$..IDS <- c(IDS, newID)     # append
  return(newID)
}

##' Add child widget to a container
##' 
##' add for a container does several things:
##' * set toplevel for each child
##' * add css to toplevel if applicable
##' * add javascript to toplevel if applicable
##' * set object into child
##' @param child child object to add
##' @param ... is not used XXX should fix this.
##' @return NULL
EXTContainer$add <- function(.,child,...) {

   ## add an ID
   child$ID <- .$newID()

   ## add parent to child for traversal
   child$parent = .
   child$toplevel = .$toplevel         # pass in toplevel window
   child$init()                        # initialize
   
   ## pass along parent properties
##XXX   child$titlename <- .$titlename

   ## Move scripts, css to toplevel
   if(!is.null(child$css)) {
     css <- .$toplevel$css
     if(is.function(child$css))
       css[[class(child)[1]]] <- list(obj = child, FUN = get("css",child))
     else if(is.character(child$css))
       css[[class(child)[1]]] <- child$css
   .$toplevel$css <- css
   }
   

   ## scripts
   if(!is.null(child$scripts)) {
     scripts <- .$toplevel$scripts
     if(is.null(scripts[[class(child)[1]]])) {
       ## not show, add
       if(is.function(child$scripts))
         scripts[[class(child)[1]]] <-
           list(obj = child, FUN = get("scripts",child))
       else if(is.character(child$scripts))
         scripts[[class(child)[1]]] <- child$scripts
       
       .$toplevel$scripts <- scripts

       ### XXX JV -- need to update for new way of handling JS .$addJSQueue...
       if(exists("..shown", envir=.$toplevel, inherits=FALSE) && .$toplevel$..shown) {
         ## need to cat this script out now,
         ## This prevents things being defined in subwindows
         ## for first time
         i <- scripts[[class(child)[1]]]
         if(is.list(i))
           i <- i$FUN(i$obj)
         ## show now
         .$Cat(i, queue=.$has_local_slot("..shown"))
       }
     }
   }
   

   
   ## add to children
   lst <- .$children
   .$children <- c(lst, child)

   if(exists("..shown",envir=., inherits=FALSE)) {
     if(!inherits(child,"gSubwindow")) {
       child$Show(queue=TRUE)
       .$addJSQueue(.$addJS(child))
     }
   }
}


##' Write javascipt code to add containers after the GUI has been shown
##'
##'  this is likely not perfect!
##' @param child gWidget instance
EXTContainer$addJS <- function(., child) {
  out <- String() +
    sprintf("%s.add(%s); %s.doLayout();",
            .$asCharacter(), child$asCharacter(), .$asCharacter())
  ## walk back tree
  toplevel <- .$toplevel
  parent <- .$parent
  while(!parent$identical(toplevel)) {
    out <- out +
      sprintf("%s.doLayout();\n", parent$asCharacter())
    parent <- parent$parent
  }
  
  return(out)
}

##' remove a widget
##'
##' @param widget widget to remove
EXTContainer$delete <- function(., widget) {
  ## remove widget from obj
  if(exists("..shown", envir=., inherits=FALSE)) {
    .$addJSQueue(.$deleteJS(widget))
  }
}

##' javascript to synchronize R with GUI
##'
##' @param widget to be removed
##' @return javascript code
EXTContainer$deleteJS <- function(., widget) {
  sprintf("%s.remove(%s);", .$asCharacter(), widget$asCharacter())
}
      

##' Set size for containers width and height are properties, not in .style
##'
##' @param value vector of width [height]
##' @return sets properties
EXTContainer$setSize <- function(., value) {
  .$width <- value[1]
  if(length(value) > 1)
    .$height <- value[2]
}

##' return size of widget
##'
##' @return integer width and height
EXTContainer$getSize <- function(.) c(width=.$width,height=.$height)

##' Create list with standard configurations
##'
##' We use a list to store configurations. This returns some defaults
EXTContainer$ExtStdCfgOptions <- function(.) {
  out <- get("ExtStdCfgOptions",EXTWidget)(.)
  out[['width']] <- .$width
  out[['height']] <- .$height
  
  ## menubar, toolbar, statusbar
  if(exists("..menuBar",envir=., inherits=FALSE)) {
    out[['tbar']] <- .$..menuBar$writeMenu()
    .$..menuBar$..shown <- TRUE
  }
  
  if(exists("..statusBar",envir=., inherits=FALSE)) {
    sbText <- String() +
      'new Ext.ux.StatusBar({' +
        'id: "' + .$ID + 'statusBar",' +
          'defaultText: "",' +
            'text:' + shQuote(.$..statusBarText) +
              '})'
    out[['bbar']] <- sbText
    .$..statusBar$..shown <- TRUE
    
  }
    
  return(out)
}



##' Show method for containers
##' Containers have children to show too.
##' also a separator is possible to display between the children,
##' although this should go
##' @param queue to we add to queue or simple cat out
EXTContainer$Show <- function(., queue=FALSE) {
  ## css -- use createStyleSheet method of Ext JS to write out
  if(exists("css",envir=., inherits=FALSE)) {
    out <- String() 
    for(i in .$css) {
      if(is.list(i))
        out <- out + i$FUN(i$obj)
      else if(is.character(i))
        out <- out + i
    }
    ## wrap in EXT JS function
    if(nchar(out)) {
      out <- String('Ext.util.CSS.createStyleSheet("') + out + '");'
      .$Cat(out, queue=queue)                        # these are printed out
    }
  }

  
  ## scripts
  if(exists("scripts", envir=., inherits=FALSE)) {
    out <- String() 
    for(i in .$scripts) {
      if(is.list(i))
        out <- out + i$FUN(i$obj)
      else if(is.character(i))
        out <- out + i
    }
    .$Cat(out, queue=queue)
  }


  ## now show container
  if(exists("..header",envir=.,inherits=FALSE))
    .$showPart(.$..header, queue=queue)
  .$showPart(.$header,queue=queue)

  
  ## write out actions if present
  if(exists("..actions", envir = ., inherits = FALSE)) {
    if(length(.$..actions) > 0) {
      for(i in .$..actions) {
        i$Show(queue=queue);
        i$..shown <- TRUE
      }
    }
  }

  

  children <- .$children
  if((n <- length(children)) > 0) {
    for(i in 1:n) {
      children[[i]]$Show(queue=queue)              # Show children
      if(i < n) {
        if(exists("..separator",envir=.,inherits=FALSE))
          .$showPart(.$..separator, queue=queue)       # widget specific
        .$showPart(.$separator, queue=queue)
      }
    }
  }

  .$show(queue=queue)                      # show self
  .$..shown <- TRUE                     # set shown

  ## handlers ## gwindow only
  ## if(exists("..setHandlers",envir=.,inherits=FALSE)) # gwindow only
  ##    .$showPart(.$..setHandlers, queue=queue)

  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer, queue=queue)
  .$showPart(.$footer, queue=queue)

}

##' shows the child items
##' 
##' items are how containers refer to their children
##' this will be overridden more than likely
EXTContainer$makeItems <- function(.) {
  childIDs <- sapply(.$children, function(i) i$ID)
  isResizable <- sapply(.$children, function(i)
                        inherits(i,"gWidgetResizable"))
  if(any(isResizable))
    childIDs[isResizable] <- paste(childIDs[isResizable],"resizer",sep="")

  
  n <- length(.$children)
  if(n == 0)
    return("{}")
  
  ## contentEl specifies where to get values
  contentEls <- paste('contentEl:', shQuote(childIDs), sep="")
  ## labels are for notebooks
  theLabels <- character(n)
  for(i in 1:n) {
    if(exists("..label",envir=.$children[[i]],inherits=FALSE))
      theLabels[i] <- String("title:") + shQuoteEsc(.$children[[i]]$..label)
  }
  ## tabTooltips are for notebooks
  tabTooltips <- character(n)
  for(i in 1:n) {
    if(exists("..tabTooltip",envir=.$children[[i]],inherits=FALSE))
      tabTooltips[i] <- String("tabTip:") + shQuoteEsc(.$children[[i]]$..tabTooltip)
  }

  itemContent <- character(n)
  for(i in 1:n) {
    if(theLabels[i] == "" && tabTooltips[i] == "")
      itemContent[i] <- paste(contentEls[i],sep=",")
    else if(theLabels[i] == "") 
      itemContent[i] <- paste(contentEls[i],tabTooltips[i],sep=",")
    else if(tabTooltips[i] == "")
      itemContent[i] <- paste(contentEls[i],theLabels[i],sep=",")
    else
      itemContent[i] <- paste(contentEls[i],theLabels[i],tabTooltips[i],sep=",")
  }
  if(!exists("makeItemsFixedItems",.,inherits=FALSE))
    .$..fixedItems <-  ""               # ends with ",".


  
  tmp <- String('{') +  .$makeItemsFixedItems

  items <- paste(paste(tmp,itemContent,'}',
                  sep=""),
            collapse=",")
  return(items)
}

##' override of EXTWidget$show,
##' 
##' unlike a EXTComponent, here we need to add in the items too
##' @param queue do we cat or queue
EXTContainer$show <- function(., queue=FALSE) {
 ## out <- String() + "\n\n" +
 ##   'o' + .$ID + '= new ' + .$ExtConstructor + '({' + '\n' +
 ##       .$mapRtoObjectLiteral(doBraces=FALSE) +
 ##         ',' + '\n' +
 ##           'items:[' +.$makeItems() +
 ##             ']' + '});' + "\n"

 out <- String() +
   sprintf("%s = new %s({\n\t%s,\n\titems:[%s]});\n",
           .$asCharacter(), .$ExtConstructor,
           .$mapRtoObjectLiteral(doBraces=FALSE),
           .$makeItems())

 ## Wanted to add dynamically, but this just doesn't work for all children (combobx, tables, ...)
 ##  out <- String() +
 ##    sprintf("%s = new %s({%s});\n",
 ##            .$asCharacter(), .$ExtConstructor,
 ##            .$mapRtoObjectLiteral(doBraces=FALSE)
 ##            )
 ## childIDs <- sapply(.$children, function(i) as.character(i$asCharacter()))
 ##  for(i in childIDs) {
 ##    out <- out + sprintf("%s.add(%s);\n", .$asCharacter(), i)
 ##  }
 ##  out <- out + sprintf("%s.doLayout();\n", .$asCharacter())

 if(!.$has_local_slot('..shown') && (.$has_local_slot("x.hidden") && .$x.hidden))
   out <- out +
     sprintf("%s.addClass('x-hidden');\n", .$asCharacter())
 
  if(.$has_local_slot("..visible"))
    out <- out + .$setVisibleJS()
  
  .$..shown <- TRUE
  .$Cat(out, queue=queue)
}

##################################################
##
## Some widget have a data store associated with them
## eg. gcombobox, gtable, gdf 

##' Main trait to hold a data store
EXTStore <- EXTWidget$new()

##' new method for a data store.
##'
##' Sets classname, toplevel widget
##' @param toplevel toplevel window
EXTStore$new <- function(., toplevel=NULL) {
  obj <- .$proto(toplevel=toplevel)
  class(obj) <- c("gWidgetStore",class(obj))
  invisible(obj)
}
## properties
## the data frame (or vector)

##' the ID for the store
EXTStore$ID <- NULL                      # get from newID
##' property. The data held in the store
EXTStore$data <- NULL
##' property. For some stores, a chosen column is designtated
EXTStore$chosenCol <- NULL               # selected column

## Store methods
##' set data into store
##'
##' @param d the data
EXTStore$setData <- function(.,d) .$data <- d

##' get Data from store
##'
##' @return the data
EXTStore$getData <- function(.) .$data

##' dimension of store
##'
##' @return dimension (from dim())
EXTStore$dim <- function(.) dim(.$getData())

##' set the chosen column property
##'
##' @param value the chosen column
EXTStore$setChosenCol <- function(.,value) .$chosenCol <- value

##' get the chosen column
##'
##' @return the value of chosen column
EXTStore$getChosenCol <- function(.).$chosenCol

##' method to coerce data to javascript array
##'
##' @param val if missing using data, otherwise will coerce this
##' @return javascript to display
EXTStore$asJSArray <- function(.,val) {
  if(missing(val))
    val <- .$data
  toJSArray(val)
}

##' method to get ID for object
##'
##' @return the ID of the object
EXTStore$asCharacter <- function(.) String('o') + .$ID + 'store'

##' which field to display. (In gcombobox)
EXTStore$displayField <- function(.) .$chosenCol

##' names of fields
##'
##' In ext "fields" are just columns. This gives  names of data
EXTStore$fieldNames <- function(.) {names(.$data)}

##' Make the javascript code to make the fields
##' 
##' for combo tihs is just an array, 
##' for a grid object it is more work
##' @return javascript string
EXTStore$makeFields <- function(.) {
  .$asJSArray(.$fieldNames())
}

##' show method for stores
##'
##' cats or queues the javascript code to show the widget
##' @param cat or queue
EXTStore$show <- function(., queue=FALSE) {
  out <- String() + "\n" +
    .$asCharacter() + '= new Ext.data.ArrayStore({' +
      'fields:  ' + .$makeFields() + ',' + '\n' +
        'data: ' + .$asJSArray() +
          '});' + '\n'
  .$Cat(out, queue=queue)
}

##' replace the store with this data
##'
##' @param data data to replace
##' @return javascript string to replace values
EXTStore$replaceStore <- function(., data) {
  if(!missing(data)) .$data <- data
  out <- String() +
    .$asCharacter() + '.removeAll();' +
      .$asCharacter() + '.loadData(' +
        .$asJSArray() + ');'
  return(out)
}
## XXX need more granular approach

## A proxy store XXX
EXTStoreWithProxy <- EXTStore$new()



##################################################
##' A proxy store will call back into the server to fetch more data.
##' We use a different handler for thes.
EXTProxyStore <- EXTStore$new()

##' initialization method for a proxy store
##'
##' A proxy store queries the server for more information. Used by
##' gbigtable and (in the future) gtree
##' @param toplevel The proxy stores are cached in the toplevel window
##' so that during the callback they can be referenced by ID.
##' @param pageSize The size of the page for a request to a data frame
##' @return a proxy store  instance
EXTProxyStore$new <- function(., toplevel=NULL, pageSize=25, ...) {
  obj <- .$proto(toplevel=toplevel, pageSize=as.numeric(pageSize))
  class(obj) <- c("gWidgetProxyStore",class(obj))
  invisible(obj)
}

##' Create javacript code to show store
##'
##' @param queue if FALSE, will cat out, otherwise queues up the javascript
##' @return NULL
EXTProxyStore$show <- function(., queue=FALSE) {
  out <- String() + "\n" +
    .$asCharacter() + '= new Ext.data.ArrayStore({' +
#    .$asCharacter() + '= new Ext.data.JsonStore({' +
#      "totalProperty: 'totalCount', root:'data'," +
      'fields:  ' + .$makeFields() + ',' + '\n' +
        'proxy: new Ext.data.HttpProxy({' +
          sprintf("url: '%s/%s/%s/%s',",  .$toplevel$..gWidgetsWWWAJAXurl,"proxystore", .$asCharacter(), .$toplevel$sessionID) +
            "method: 'POST'" +         # use POST, not GET as this makes processing easier
              "})" +
                  "})" + "\n"
  out <- out +
    sprintf("%s.getTotalCount = function() {return %s};", .$asCharacter(), nrow(.$data))
  
  .$Cat(out, queue=queue)
}

##' Parse the query and return data as json
##'
##' Very ext specific. Raise error if not correct
##' @param query a list, typically the POST variable
##' @return the data in javascript or json encoded form
EXTProxyStore$parseQuery <- function(., query) {
  df <- .$getData()
  m <- nrow(df)
  if(!is.null(query$start)) {
    ## start limit query
    start <- as.numeric(query$start); limit <- as.numeric(query$limit)
    if(m == 0 || m < start) {
      stop("Data store wrong size for request")
    } else {
      ind <- seq(start, min(m, start+limit))
      out <- toJSArray.data.frame(df[ind,,drop=FALSE]) # not .$toJSArray
#      out <- ourToJSON(df[ind,,drop=FALSE])
#      out <- sprintf("{'totalCount':'%s', 'data':%s}", nrow(df), out)
    }
  } else {
    out <- ""
  }
  return(out)
}

##' javascript code to replace data
##'
##' Adds in getTotalCount redeifintion
##' @param data Replaces data in store and updates total count. 
##' @TODO The latter is useful for arrays it may need to be moved out to a subclass
EXTProxyStore$replaceData <- function(., data) {
  out <- get("replaceData", "EXTStore")(., data)
  out <- out +
    sprintf("%s.getTotalCount = function() {return %s};", .$asCharacter(), nrow(.$data))
  out
}

### Proxy Tree Store
##' A proxy tree store
EXTProxyTreeStore <- EXTStore$new()

##' new method. Needs toplevel, like proxystore
EXTProxyTreeStore$new <- function(., toplevel=NULL, ...) {
  obj <- .$proto(toplevel=toplevel)
  class(obj) <- c("gWidgetProxyTreeStore",class(obj))
  invisible(obj)
}

##' Create javacript code to show store
##'
##' @param queue if FALSE, will cat out, otherwise queues up the javascript
##' @return NULL
EXTProxyTreeStore$show <- function(., queue=FALSE) {
  if(!exists("gWidgetsWWWAJAXurl") || is.null(gWidgetsWWWAJAXurl))
    gWidgetsWWWAJAXurl <- getOption("gWidgetsWWWAJAXurl")
  if(is.null(gWidgetsWWWAJAXurl))  {
    gWidgetsWWWAJAXurl <- "/gWidgetsWWW"
  }

  out <- String()
  
  .$Cat(out, queue=queue)
}

## tree passes in id We need to compute based on that and return a value looking like:
##     [{
##     "text": "adapter",
##     "id": "source\/adapter",
##     "cls": "folder"
## }, {
##     "text": "dd",
##     "id": "source\/dd",
##     "cls": "folder"
## }, {
##     "text": "debug.js",
##     "id": "source\/debug.js",
##     "leaf": true,
##     "cls": "file"
## }]

##' Parse the query and return data as json
##'
##' Very ext specific. Raise error if not correct
##' @param query a list, typically the POST variable
##' @return the data in javascript or json encoded form, set the ..data variable to a list

EXTProxyTreeStore$parseQuery <- function(., query) {
  df <- .$getData()
  m <- nrow(df)

  ## kludgy bit to put in icons
  si <- getStockIcons()
  hasIcon <- function(i) i %in% names(si)
  
  makeItemWithIcon <- function(base, id, leaf=FALSE,  icon="",  value) {
    sprintf('{"text":"%s","id":"%s:%s","leaf":%s, "icon":"%s"}',
            ifelse(missing(value), id, paste(id, paste(value, collapse="\t"), sep="\t")),
            base, id,
            tolower(as.character(!leaf)),
            ifelse(hasIcon(icon), si[icon], si["blank"])
            )
  }
  makeItem <- function(base, id, leaf=FALSE, value) {
    sprintf('{"text":"%s","id":"%s:%s","leaf":%s}',
            ifelse(missing(value), id, paste(id, paste(value, collapse="\t"), sep="\t")),
            base, id,
            tolower(as.character(!leaf))
            )
  }
  if(!is.null(query$node)) {
    path <- strsplit(query$node, ":")[[1]][-1]         # strip off 1
    .$..path <- path
    .$..node <- query$node
    odata <- get("..offspring.data", .)
    children <- get("..offspring",.)(path, odata)         # returns data frame: id, offspring, [icon], text
    m <- nrow(children)
    if(m == 0) {
      out <- "[]"
    } else {
      icon.FUN <- .$..icon.FUN
      if(!is.null(icon.FUN)) {
        if(is.function(icon.FUN)) {
          icons <- icon.FUN(children)
        } else {
          icons <- children[,3]
          children[[3]] <- NULL
        }
      }
      
      out <- sprintf("[%s]", paste(sapply(1:m, function(i) {
        if(is.null(icon.FUN)) {
          if(ncol(children) > 2)
            makeItem(query$node, children[i,1], children[i,2], children[i,-(1:2)])
          else
            makeItem(query$node, children[i,1], children[i,2])
        } else {
          if(ncol(children) > 2)
            makeItemWithIcon(query$node, children[i,1], children[i,2], icons[i], children[i,-(1:2)])
          else
            makeItemWithIcon(query$node, children[i,1], children[i,2], icons[i])
        }
      }), collapse=","))
    }
    out
  } else {
    out <- ""
    .$..path <- character(0)                     # the path
    .$..node <- character(0)
  }
  return(out)
}




##################################################
##' Base trait for components with stores
##' 
##' extends Component to handle a data store
EXTComponentWithStore <- EXTComponent$new()

## additional properties

## property store -- holds an EXTStore instance
EXTComponentWithStore$..store <- NULL

## methods

##' Assign Value -- clicks sends back rowindex
##' 
##' @param . self
##' @param value value, list with initial component the row number
EXTComponentWithStore$assignValue <- function(., value) {
  .$..data <- as.numeric(value[[1]])
}

##' Get value (savlue)
##'
##' @param index logical. If TRUE return index
##' @param drop logical. If TRUE drop dimensions when possible
##' @return the main value associated with the widget
EXTComponentWithStore$getValue <- function(., index=NULL, drop=NULL,...) {
  ## we store value as an index
  out <- .$..data
  values <- .$..store$data

  ## hack to make chosenCol work with combobox
  chosenCol <- getWithDefault(.$..store$chosenCol, 1)
  if(is.character(chosenCol) && !(chosenCol %in% names(values)))
    chosenCol <- 1

  if(!is.numeric(out)) {
    if(any(tmp <- out == values[,chosenCol]))
      out <- min(which(tmp))
    else
      return(out)                         # a character not in store
  }
  out <- as.numeric(out)


  
  ## no index -- return values
  if(!is.null(index) && index) {
    return(out)
  } else {

    if(is.null(drop) || drop) {
      return(values[out, chosenCol, drop=TRUE])
    } else {
      return(values[out,])
    }
  }      
}


##' getValue method for component with stores
##'
##' @returns values in store dropping __index
EXTComponentWithStore$getValues <- function(., ...) {
  tmp <- .$..store$data
  if(names(tmp)[1] == "__index")
    tmp <- tmp[,-1, drop=FALSE]
  tmp
}

##' length of items in store
##'
##' @returns length
EXTComponentWithStore$getLength <- function(.)
  length(.$getValues())

##' size of data frame
EXTComponentWithStore$.dim <- function(.) dim(.$getValues())

##' names of values in store
##'
##' @return character names
EXTComponentWithStore$getNames <- function(.)
  names(.$getValues())

## XXX names<- not defined

##' getValue
##'
##' ..data holds indices, here we can return either
EXTComponentWithStore$getValue <- function(.,index=NULL ,drop=NULL,...) {
  ## we store value as an index
  out <- .$..data
  if(!is.null(index) && index) {
    return(as.numeric(out))
  } else {
    ## depends on drop
    values <- .$getValues()
    if(is.null(drop) || drop) {
      return(values[as.numeric(out),.$..store$chosenCol,drop=TRUE])
    } else {
      return(values[as.numeric(out),])
    }
  }      
}


##' setValue in widget. Values stored are the indices that are selected
EXTComponentWithStore$setValue <- function(., index=NULL, ..., value) {
  if(.$has_local_slot("..setValue")) {
    .$..setValue(index=index, ..., value=value)
  } else{
    index <- getWithDefault(index, FALSE)
    if(index) {
      .$..data <- as.integer(value)
    } else {
      ## must match value against first column
      values <- .$getValues()[,.$..store$chosenCol,drop=TRUE]
      tmp <- unique(match(value, values))
      .$..data <- tmp[!is.na(tmp)]
    }
  }
  ## now process if shown
  if(.$has_local_slot("..shown"))
    .$addJSQueue(.$setValueJS(index=index, ...))

}

##' Synchronize the values in the R widget with the GUI
##'
##' @param . object
##' @param ... passed to serValueJS of any instance overrides
EXTComponentWithStore$setValueJS <- function(., ...) {
    if(.$has_local_slot("..setValueJS"))
      .$..setValueJS(...)
  
  ind <- sort(.$getValue(index=TRUE, drop=TRUE))
  if(length(ind) == 0 || ind[1] <= 0)
    out <- sprintf("%s.clearValue()", .$asCharacter())
  else
    out <- sprintf("%s.getSelectionModel().selectRows(%s);", .$asCharacter(), toJSON(ind-1))

  return(out)
}

##' Set values in store
##'
##' @param i index XXX not implemented
##' @param j index XXX not implemented
##' @param ... passed to setValueJS
##' @param value values to store
EXTComponentWithStore$setValues <- function(.,i,j,...,value) {
  ## XXX need to include i,j stuff
  items <- cbind("__index"=seq_len(nrow(value)), value)
  .$..store$data <- value
  if(.$has_local_slot("..shown"))
    .$addJSQueue(.$setValuesJS(...))
}

##' produce javascript to synchronize R with GUI
##'
##' @return javascript
EXTComponentWithStore$setValuesJS <- function(., ...) {
  if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)
  
  out <- String() +
    .$..store$asCharacter() + '.removeAll();' +
      .$..store$asCharacter() + '.loadData(' +
        .$asJSArray(.$..store$data) +');'

  return(out)
}

##' Write out transport value part.
##'
##' Just defines the value variable in javascript to pass back to R via _transportToR
EXTComponentWithStore$transportValue <- function(.,...) {
  ## we packed in __index so we can get the index even if we've sorted
  if(.$has_local_slot("..multiple") &&.$..multiple) {
    ## work a bit to get the value
    out <- String() +
      paste('var store = w.getStore();',
            'var selModel = w.getSelectionModel();',
            'var values = selModel.getSelections();',
            'var value = new Array();', # value is return value
            'for(var i = 0, len=values.length; i < len; i++) {',
            '  var record = values[i];',
            '  var data = record.get("__index");',
            '  value[i] = data',
            '};',
            sep="")
  } else {
    out <- String() +
      paste('var record = w.getStore().getAt(rowIndex);',
            'var value = record.get("__index");',
            sep="")
  }
  return(out)
}


##' set the size of component with store.
##'
##' @param value Can be a vector, as usual, or a list. If the latter,
##' the components width, height and columnWidths are of interest. The
##' latter to set the column widths -- in pixels -- as opposed to
##' having it determined
##' @return NULL
EXTComponentWithStore$setSize <- function(., value) {
  if(is.list(value)) {
    width <- value$width
    height <- value$height
    columnWidths <- value$columnWidths
  } else {
    width <- value[1]
    height <- value[2]
    columnWidths <- NULL
  }
  get("setSize", EXTWidget)(., c(width,height))
  if(!is.null(columnWidths))
    .$..columnWidths <- rep(columnWidths, length.out=.$getLength())

  ## Has local slot shown ...if(
}


##' visible<- is not implemented, use \code{$filter} proto instead
EXTComponentWithStore$setVisibleJS <- function(., ...) {}

##' filter is used to filter our values matching regular expression in the given column
##' @param . component with store
##' @param colname name of column to match regular expression agains
##' @param regex regular expression to match against, If empty, skip
##' @return javascript code to produce the changes is added to the
##' queue. This is called only after GUI is rendered.
EXTComponentWithStore$filter <- function(., colname, regex) {
    if(!exists("..shown",envir=., inherits=FALSE)) {
      ## "Can only filter once object is shown"
      out <- ""
    }

    if(missing(colname) || !colname %in% names(.$..store$data))  {
       ## Need colname to match one of the names of the data set
      out <- ""
    }

    ## should check for regex via ^/(.*)/$ but instead we just assume a regular expression
    if(missing(regex) || regex=="") {
      out <- sprintf("o%s.getStore().clearFilter();", .$ID)
    } else {
      out <- sprintf("o%s.getStore().filter('%s',RegExp('%s'));", .$ID, colname, regex)
    }
    .$addJSQueue(out)
  }

##' visibility
##'
##' Use filter method and initil ..inaex column to implment filtering
##' @param . self
##' @param value Logical, recycled to number of rows. TRUE for rows to display
EXTComponentWithStore$setVisible <- function(., value) {
  n <- dim(.)[1]
  value <- rep(value, length.out=n)
  .$..visible <- value                # XXX???
  inds <- which(value)
  reg <- paste("^",inds,"$", sep="", collapse="|")
  .$filter("__index", reg)
}



##' Wrapper to turn an object into an JS array
##'
##' @param ... passed to store's methods
EXTComponentWithStore$asJSArray <- function(.,...) {
  .$..store$asJSArray(...)
}

##' Show method for component.
##'
##' show needs to show store and component
##' @param queue if FALSE will cat out result otherwise queues it
##' @return NULL
EXTComponentWithStore$show <- function(., queue=FALSE) {
  .$..store$show(queue=queue)
  get("show",EXTComponent)(., queue=queue)       # call up
}

##' make the column model for display
##'
##' This creates meta information about the store needed by ext
##' @return javascript code
EXTComponentWithStore$makeColumnModel <- function(.) {
    ## return array for columns
    ## id, header, sortable, renderer, dataIndex, tooltip
##     columns: [
##               {id:'company',header: "Company", sortable: true, dataIndex: 'company'},
##               {header: "Price",  sortable: true, renderer: 'usMoney', dataIndex: 'price'},
##               {header: "Change", sortable: true, renderer: change, dataIndex: 'change'},
##               {header: "% Change", sortable: true, renderer: pctChange, dataIndex: 'pctChange'},
##               {header: "Last Updated", sortable: true, renderer: Ext.util.Format.dateRenderer('m/d/Y'), dataIndex: 'lastChange'}
##               ],

    mapRenderer <- function(type) {
      switch(type,
             "character"="",
             "String" = "",
             "integer" = ",renderer:gtableInteger",
             "numeric" = ",renderer:gtableNumeric",
             "logical" = ",renderer:gtableLogical",
             "factor" = "",
             "icon" = ",width: 16,renderer:gtableIcon",              # for icons(we create this)
             "date" = ",renderer:gtableDate",               # we create this?
             "")
    }

    df <- .$..store$data
    renderers <- sapply(df[,-1, drop=FALSE], function(i) mapRenderer(class(i)[1]))
    colNames <- names(df)[-1]           # XXX
    colNames <- shQuoteEsc(colNames)

    ## widths
    if(.$has_slot("..columnWidths")) {
      colWidths <- .$..columnWidths
    } else {
      
      fontWidth <- 10
      colWidths <- sapply(df[,-1, drop=FALSE], function(i) {
        if(length(i))
           max(nchar(as.character(i))) + 1
        else
          20
      })
      colWidths <- pmax(colWidths, nchar(names(df[,-1, drop=FALSE])) + 1)
      totalWidth <- ifelse(exists("..width", envir=., inherits=FALSE), .$..width, "auto")
      if(totalWidth == "auto" || fontWidth * sum(colWidths) > totalWidth)
        colWidths <- colWidths * fontWidth       # fontWidth pixels per character
      else
                                        #      colWidths <- floor(fontWidth * colWidths * totalWidth/sum(colWidths))
        colWidths <- colWidths * fontWidth       # fontWidth pixels per character
    }
    ## didn't work for header:
    trimDD <- function(x) {
      ind <- grep("^..", x)
      if(length(ind) > 0)
        x[ind] <- "''"
      return(x)
    }

    tmp <- paste('{',
                 'id:',colNames,
                 ', header:',colNames,
                 ', sortable:true',
                 ', width:', colWidths,
                 ', dataIndex:',colNames,
                 renderers,
                 '}',
                 sep="")
    out <- paste('[\n', paste(tmp,collapse=",\n"), ']', collapse="")

    return(out)
  }


##' Make fields for the store
##'
##' Makes javascript code to specify the fields
##' @return javascript code
EXTComponentWithStore$makeFields <- function(.) {
  ## return something like this with name, type
  ##     fields: [
  ##            {name: 'company'},
  ##            {name: 'price', type: 'float'},
  ##            {name: 'change', type: 'float'},
  ##            {name: 'pctChange', type: 'float'},
  ##            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'}
  ##         ]
  ## types in DataField.js
  mapTypes <- function(type) {
    switch(type,
           "character"="",
           "String" = ",type: 'string'",
           "integer" = ",type: 'int'",
           "numeric" = ",type: 'float'",
           "logical" = ",type: 'boolean'",
           "factor"  = "",
           "date" = ",type:date",
           "")
  }
  df <- .$..store$data
  types <- sapply(df[,-1, drop=FALSE], function(i) mapTypes(class(i)[1]))
  colNames <- shQuoteEsc(names(df)[-1])
  tmp <- paste("{name:", colNames, types, "}", sep="")
  out <- paste("[",tmp,"]", collapse="\n")
  
  return(out)
}

##' Property. Transport signal -- when to send back info
##' This does cell click (for gtable, gbigtable) 
EXTComponentWithStore$transportSignal <- c("cellclick")

##' method to transport values back to R
##'
##' Javascript to get value from widget passes back to R session
##' @param ... ignored
##' @return javascript string
EXTComponentWithStore$transportValue <- function(.,...) {
    ## we packed in __index so we can get the index even if we've sorted
    if(.$..multiple) {
       ## work a bit to get the value
       out <- String() +
         'var store = w.getStore();' +
           'var selModel = w.getSelectionModel();' +
             'var values = selModel.getSelections();' +
               'var value = new Array();' +
                 'for(var i = 0, len=values.length; i < len; i++) {' +
                   'var record = values[i];' +
                     'var data = record.get("__index");' +
                         'value[i] = data' +
                           '};'
     } else {
       out <- String() +
         'var record = w.getStore().getAt(rowIndex);' +
           'var value = record.get("__index");' 
     }
    return(out)
  }

##' method to add a click handler
##'
##' @param handler a gWidgets type handler
##' @param action passed to handler
##' @param ... ignored
##' @return code to add handler
EXTComponentWithStore$addHandlerClicked <- function(.,handler, action=NULL, ...) {
  ## we need to set up some stuff
  .$addHandler(signal="cellclick",
               handler = handler,
               action = action,
               handlerArguments = "grid, rowIndex, colIndex, e",
               handlerValue = "var value = rowIndex + 1;"
               )
}

##' Method to add double click handler
##'
##' @param handler a gWidgets type handler
##' @param action passed to handler
##' @param ... ignored
##' @return code to add handler
EXTComponentWithStore$addHandlerDoubleclick  <- function(.,handler, action=NULL, ...) {
  ## we need to set up some stuff
  .$addHandler(signal="dblclick",
               handler = handler,
               action = action,
               handlerArguments = "grid, rowIndex, colIndex, e",
               handlerValue = "var value = rowIndex + 1;")
}
 
  
##################################################
  
##' Sub Trait for gdf -- allows editing
EXTComponentDfStore <- EXTComponentWithStore$new()


##' set values in store; ([<-)
##'
##' @param i row index
##' @param j column index
##' @param ... ignored
##' @param value value to set
##' @return sets the values in the store and if needed add javascript to queue
EXTComponentDfStore$setValues <- function(., i, j, ..., value) {

  if(missing(i))  i <- seq_len(nrow(.$..store$data))
  if(missing(j))  j <- seq_len(ncol(.$..store$data))

  d <- .$..store$getData()
  d[i,j] <- value
  .$..store$setData(d)
  if(exists("..shown", envir=., inherits=FALSE)) {
    .$addJSQueue(.$setValuesJS(i,j,value=value))
  }
}

##' write java script to set the values
##'
##' Called by setValues method to write out javascript
##' @param i row index
##' @param j column index
##' @param value value to set
##' @return javascript code is queued up
EXTComponentDfStore$setValuesJS <- function(., i,j,..., value) {
  if(missing(i) && missing(j)) {
    .$..store$replaceStore()
    return()
  }
  ## make value have i,j
  if(!is.matrix(value) || !is.data.frame(value))
    value <- data.frame(value=value, stringsAsFactors=FALSE)
  
  ## set i,j elements of store
  ## get record (getAt)
  ## set record by column name
  ## commit record
  out <- String() + "\n"
  for(row in seq_along(i)) {
    out <- out + sprintf("rec = %s.getAt(%s);", .$..store$asCharacter(), i[row] - 1)
    for(col in seq_along(j)) {
      out <- out + sprintf("rec.set('%s', '%s');", names(.)[j[col]], escapeQuotes(value[row,col]))
    }
    out <- out + "rec.commit();" + "\n"
  }

  .$addJSQueue(out)
}
  

##################################################

##' Extend ComponentWithStore to handl proxy stores
EXTComponentWithProxyStore <- EXTComponentWithStore$new()

##' property store -- holds an EXTStore instance
EXTComponentWithProxyStore$..store <- NULL

##' show method for Proxy stores
##'
##' Adds a call to load method of show
EXTComponentWithProxyStore$show <- function(., queue=FALSE) {
  .$..store$show(queue=queue)
  get("show",EXTComponentWithStore)(., queue=queue)       # call up
  .$Cat(sprintf("%s.load({params:{start:0, limit:%s}});",
                .$..store$asCharacter(), .$..store$pageSize),
        queue=queue)
}

## Tree STore
##' Extend ComponentWithStore to handl proxy stores
EXTComponentWithProxyTreeStore <- EXTComponentWithStore$new()

##' property store -- holds an EXTStore instance
EXTComponentWithProxyTreeStore$..store <- NULL

##' show method for Proxy stores
##'
##' Adds a call to load method of show
EXTComponentWithProxyTreeStore$show <- function(., queue=FALSE) {
  .$..store$show(queue=queue)
  get("show",EXTComponentWithStore)(., queue=queue)       # call up
  .$Cat(sprintf("",""), queue=queue)
}

##' method to add a click handler
##'
##' @param handler a gWidgets type handler
##' @param action passed to handler
##' @param ... ignored
##' @return code to add handler
EXTComponentWithProxyTreeStore$addHandlerClicked <- function(.,handler, action=NULL, ...) {
  ## we need to set up some stuff
  .$addHandler(signal="click",
               handler = handler,
               action = action,
               handlerArguments = "node, e",
               handlerValue = "var value = node.id;"
               )
}

##' Method to add double click handler
##'
##' @param handler a gWidgets type handler
##' @param action passed to handler
##' @param ... ignored
##' @return code to add handler
EXTComponentWithProxyTreeStore$addHandlerDoubleclick  <- function(.,handler, action=NULL, ...) {
  ## we need to set up some stuff
  .$addHandler(signal="dblclick",
               handler = handler,
               action = action,
               handlerArguments = "node, e",
               handlerValue = "var value = node.id;")
}
 

### Components in  a panel ##################################################

### Some widgets render better in a panel
## This overrides the writeConstructor method to show the object
## in ExtStdCfgOptions use an xtype and override renderTo with NULL
## see gcheckbox for an example

##' Base trait for a component in a panel container. Used for some widgets to render better.
EXTComponentInPanel <- EXTComponent$new()

##' get item ID
##'
##' Different from asCharacter call, as that is ID of panel not component
##' @return character ID of item 
EXTComponentInPanel$getItemID <- function(.) String(.$ID) + 'item'

##' override of writeConstructor method, called by show
##' @return javascript code to write out the constructor
EXTComponentInPanel$writeConstructor <- function(.) {
  lst <- list(id = as.character(.$ID),
              xtype = "panel",
              layout = "fit",
              border = FALSE,
              hideBorders = TRUE,
              width = ifelse(exists("..width", ., inherits=FALSE),
                .$..width,"auto"),
              renderTo = String(.$toplevel$..renderTo), #String("Ext.getBody()"),
              items = String("[") + .$mapRtoObjectLiteral() + ']'
              )
  out <- String() + "\n" +
    'o' + .$ID + 'panel = new Ext.Panel(' + # no var -- global
      .$mapRtoObjectLiteral(lst) +
        ');' + '\n'

 if(!.$has_local_slot('..shown') && (.$has_local_slot("x.hidden") && .$x.hidden))  
   out <- out +
     sprintf("%s.addClass('x-hidden');\n", .$asCharacter())
  
  ## get component from first child object
  out <- out +
    'o' + .$ID + ' = ' +                # no var -- global
      'o' + .$ID + 'panel.getComponent("' + .$getItemID() + '");' + '\n'
  return(out)
}

##' Base trait for a component with items like gradio and gcheckboxgroup
##' 
##' we use a panel and use the items to store the values
##' the handlers need to be assigned to each 
EXTComponentWithItems <- EXTComponent$new()

##' propoerty xtype property,
EXTComponentWithItems$xtype <- ""       # eg "checkbox", "radio"
##' property itemname
EXTComponentWithItems$itemname <- "item"
##' property Which ext constructor to use
EXTComponentWithItems$ExtConstructor <- "Ext.Panel"
## ##' property. The x.hidden property, when TRUE, will first hide widget
EXTComponentWithItems$x.hidden <- FALSE

##' assign value
EXTComponentWithItems$assignValue <- function(., value) {
  svalue(., index=NULL) <- value[[1]]
}



##' Is i checked
##'
##' @return logical indicating if item i is checked
EXTComponentWithItems$checked <- function(.,i) {
  ## return TRUE if checked o/w false
}
##' Make the items for display. Called by show method
##'
##' @return javascript code to make the items
EXTComponentWithItems$makeItems <- function(.) {
  out <- String()
  
  values <- .$getValues()
  if((n <- length(values)) < 2)  return(out)
    
  tmp <- list()                          # store items as String
  for(i in 1:n) {
    lst <- list(xtype = .$xtype,
                name = as.character(String() + .$ID + .$itemname),
                boxLabel = as.character(values[i]),
                checked = .$checked(i)
                )
    tmp[[i]] <- .$mapRtoObjectLiteral(lst)
  }

  out <- out +
    '[' + paste(tmp,collapse=",") + ']'
  
  return(out)
}

##' Write out javascript handlers.
##'
##' Must add to each item, not just one
##' @return javascript code
EXTComponentWithItems$writeHandlersJS <- function(.) {
  if(exists("..handlers", envir=., inherits=FALSE))
    allHandlers <- .$..handlers
  else
    allHandlers <- list()

  ## get all signals
  signals <- c()
  if(!is.null(.$transportSignal))
    signals <- .$transportSignal
  if(length(allHandlers) > 0)
    signals <- union(signals, names(allHandlers))

  if(length(signals) == 0) return(String(""))     # nothing to do
  
  out <- String()
  for(sig in signals) {
    for(i in 1:(n <- length(.))) {
      out <- out +
        paste(sprintf("var widget = %s.getComponent(%s);",.$asCharacter(), as.character(i-1)),
              sprintf("widget.on('%s', function(%s) {%s}, this, {delay:1, buffer:1, single:false});",
                      sig,
                      .$handlerArguments(sig),
                      ifelse(!is.null(.$transportSignal) && sig %in% .$transportSignal,
                             .$writeTransport(ext = shQuote(i), signal=sig),
                             "")
                      ),
              sep="")
      ## out <- out +
      ##   'var widget = ' + .$asCharacter() + '.getComponent(' +
      ##     as.character(i - 1) + ');' +
      ##       'widget.on(' +
      ##         ## XXX transport args needs to be siganl dependent!!
      ##         shQuote(sig) + ','  +
      ##           'function(' + .$handlerArguments(sig) + ') {\n'

      ## ## write out transport if necessary
      ## ## XXX code to pass values createDelegate ....
      ## if(!is.null(.$transportSignal) && sig %in% .$transportSignal) {
      ##   out <- out + .$writeTransport(ext = shQuote(i), signal=sig) # ## pass this in
      ## }
      ## out <- out +'}' +
      ##   ',this, {delay:1,buffer:1, single:false});' + '\n'
      

      ## write out handler if needed
      if(!is.null(allHandlers[[sig]])) {
        handler <- allHandlers[[sig]]
        out <- out +
          paste(sprintf("var widget = %s.getComponent(%s);", .$asCharacter(), as.character(i-1)),
#                sprintf("widget.on('%s', function(%s) {%s}, this, {delay:1, buffer:1, single:false});",
                sprintf("widget.on('%s', %s, this, {delay:1, buffer:1, single:false});",
                        sig,
                        .$writeHandlerFunction(signal=sig, handler=handler)),
                sep="")
##           'var widget = ' + .$asCharacter() + '.getComponent(' + as.character(i - 1) + ');' +
##             'widget.on(' +
##             ## XXX transport args needs to be siganl dependent!!
##             shQuote(sig) + ',' +
## #              'function(' + .$handlerArguments(sig) + ') {\n' +
##                 .$writeHandlerFunction(signal=sig, handler=handler) +
##                   '\n'
##         ##           'runHandlerJS(' + handler$handlerID  +
##         ##             handler$handlerExtraParameters + ');' + '\n' +
##         ##               'true;' + '\n'
##         out <- out +
## #          '}' +
##             ',this, {delay:100,buffer:100, single:false});' + '\n'
      }
    }
  }

  return(out)
}






##############################
## gwidget methods

##' Generic to get primary value for widget
svalue <- function(obj,index=NULL, drop=NULL,...) UseMethod("svalue")


##' gWidget implementation of svalue method
##'
##' Calls getValue method
svalue.gWidget <- function(obj,index=NULL, drop=NULL,...) {
  obj$getValue(index=index,drop=drop,...)
}

##' Generic to set primary value for a widget
"svalue<-" <- function(obj,index=NULL, ...,value) UseMethod("svalue<-")

##' gWidget class implementation
##'
##' Calls setValue method
"svalue<-.gWidget" <- function(obj,index=NULL, ..., value) {
  obj$setValue(index=index,..., value=value)
  return(obj)
}

##' add is used by gtext atleast. $add implicitly used by contaienrs
"add" <- function(obj,value,...) UseMethod("add")

##' gWidget lcass add method.
##'
##' Calls add method
"add.gWidget" <- function(obj, value, ...) {
  if(exists("add",envir=obj, inherits=TRUE))
    obj$add(child=value,...)
}

## delete removes add -- in this case we hide
"delete" <- function(obj, widget, ...) UseMethod("delete")
delete.gWidget <- function(obj, widget, ...) {
  if(exists("delete",envir=obj, inherits=TRUE))
    obj$delete(widget,...)
}
  

## insert is new name for add for gtext
"insert" <- function(obj, value, where = c("end","beginning","at.cursor"),
                     font.attr = NULL,
                     do.newline = TRUE, ...) UseMethod("insert")
"insert.gWidget" <- function(obj, value, where = c("end","beginning","at.cursor"),
                             font.attr = NULL,
                             do.newline = TRUE, ...) {
  where = match.arg(where)
  add(obj, value, where=where, font.attr=font.attr, do.newline=do.newline,...)
}
                       

## toggle whether widget can receive input
"enabled" <- function(obj) UseMethod("enabled")
"enabled.gWidget" <- function(obj) {
  . <- obj
  if(exists("..enabled", envir=., inherits =FALSE))
    return(.$..enabled)
  else
    return(TRUE)
}
"enabled<-" <- function(obj,...,value) UseMethod("enabled<-")
"enabled<-.gWidget" <- function(obj,..., value) {
  . <- obj
  .$setEnabled(value)

  obj
}


## dispose of widget. We simply hide it here
## no method until created
"dispose" <- function(obj,...) UseMethod("dispose")
"dispose.gWidget" <- function(obj,...) {
  . = obj

  if(exists("dispose", envir=.)) {
    .$dispose()
  } else if(exists("..shown",envir=., inherits=FALSE)) {
    .$addJSQueue(.$callExtMethod("hide"))
  }
}

 ## focus
"focus<-" <- function(obj,...,value) UseMethod("focus<-")
"focus<-.gWidget" <- function(obj,..., value) {
  . = obj
  value <- as.logical(value)
  ## set ..focus attribute -- not implemented
  if(value) .$..focus <- TRUE
  
  if(exists("..shown",envir=., inherits=FALSE) && value)
    .$addJSQueue(.$callExtMethod("focus",tolower(as.character(value))))

  return(obj)
}

## tag
tag <- function(obj, key, ...) UseMethod("tag")
tag.gWidget <- function(obj, key, ...)  {
  attr(obj, key)
}
"tag<-" <- function(obj, key,...,value) UseMethod("tag<-")
"tag<-.gWidget" <- function(obj, key, ..., value) {
  attr(obj, key) <- value
  obj
}

## id
id <- function(obj, ...) UseMethod("id")
id.gWidget <- function(obj, ...) obj$asCharacter()

## visible, visible<-
visible <- function(obj) UseMethod("visible")
visible.gWidget <- function(obj) obj$getVisible()
"visible<-" <- function(obj,...,value) UseMethod("visible<-")
"visible<-.gWidget" <- function(obj,..., value) {
  obj$setVisible(value)
  return(obj)
}



"[.gWidget" <- function(x,i,j,drop = TRUE) {
##  if (missing(i)) TRUE else length(cols) == 1) {
  . = x
  values <- .$getValues()
  
  if(missing(i)) {
    if(is.null(dim(values)))
      return(values)
    else if(missing(j))
      return(values[,,drop=drop])
    else
      return(values[,j,drop=drop])
  } else {
    if(is.null(dim(values)))
      return(values[i])
    else if(missing(j))
      return(values[i,,drop=drop])
    else
      return(values[i,j,drop=drop])
  }
}

"[<-.gWidget" <- function(x,i,j,...,value) {
  . = x
  if(missing(i) && missing(j))
    .$setValues(..., value=value)
  else if(missing(i))
    .$setValues(j =j,..., value=value)
  else if(missing(j))
    .$setValues(i = i,..., value=value)
  else
    .$setValues(i = i, j =j,..., value=value)
  return(x)
 }

## names
"names.gWidget" <- function(x) {
  . <-  x
  .$getNames()
}
"names<-.gWidget" <- function(x, value) {
  . = x
  .$setNames(value)
  return(x)
}


## size of widget
"size" <- function(obj) UseMethod("size")
"size.gWidget" <- function(obj) {
  obj$getSize()
}
"size<-" <- function(obj,value) UseMethod("size<-")
"size<-.gWidget" <- function(obj,value) {
  obj$setSize(value)
  return(obj)
}

## set font -- use stylesheet
## eg:
## font(l) <- c("font-family"="Verdana, Arial, Helvetica, sans-serif",
## 	   "font-size" = "large",
## 	   "font-style" = "italic",
## 	   "font-weight" = "bold")
## cf: http://www.yourhtmlsource.com/stylesheets/csstext.html
"font<-" <- function(obj,value) UseMethod("font<-")
"font<-.gWidget" <- function(obj,value) {
  . <- obj
  ## gWidgets names are family, size, style, weigth, ala X11
  changeThese <- c("family","size","style","weight")
  
  vals <- intersect(names(value), changeThese)
  for(i in vals)
    names(value)[which(names(value) == i)] <- paste("font-",i,sep="")

  if(!exists("..style",envir=., inherits=FALSE))
    .$..style <- value
  else
    .$..style <- c(.$..style,value)
  
  if(exists("..shown",., inherits=FALSE)) {
    .$addJSQueue(.$setStyleJS())
   }
  
  return(obj)
}

## we use print for gwindow and gsubwindow as a more natural alias
## than $Show() i.e. after storing objects into the toplevel window or
## its subwindos, we need to be able to show the contents to the
## brower. This is done with the proto method $#Show(), which here is
## aliased to theprint method of the proto objects.
print.gWindow <- print.gSubwindow <- function(x,...) {
  . = x;
  .$Show()
}
  


## Method to set a tooltip on an object
## if isURL(value) == TRUE, then loads from website
## value can be a list with title, message or just a message
"tooltip<-" <- function(obj,value) UseMethod("tooltip<-")
"tooltip<-.gWidget" <- function(obj,value) {
  if(isURL(value)) {
    obj$..tooltip <- value
  } else {
    obj$..tooltip <- value
  }
  return(obj)
}

## addSpring and addSpace are used to align children within the group
## containers. These are not defined, but should be in ggroup.R
addSpace <- function(obj, value, horizontal=TRUE, ...) UseMethod("addSpace")
addSpace.gWidget <- function(obj, value, horizontal=TRUE, ...) 
  obj$addSpace(value, horizontal = horizontal, ...)

addSpring <- function(obj, ...) UseMethod("addSpring")
addSpring.gWidget <- function(obj,  ...) 
  obj$addSpring(...)



## DND -- XXX not defined
addDropSource <- function(obj, targetType = "text", handler = NULL, action = NULL, ...) UseMethod("addDropSource")
addDropMotion <- function(obj, handler = NULL, action = NULL, ...) UseMethod("addDropMotion")
addDropTarget <- function(obj, targetType = "text", handler = NULL, action = NULL,  ...) UseMethod("addDropTarget")


## always true, but gWidgets methods
isExtant <- function(x,...) UseMethod("isExtant")
isExtant.gWidget <- function(x,...) TRUE


blockHandler <- function(obj, ID=NULL, ...) UseMethod("blockHandler")
blockHandler.gWidget <- function(obj, ID=NULL, ...) {
  w <- obj$toplevel
  if(is.null(ID))
    ID <- seq_along(w$jscriptHandlers)
  ID <- as.numeric(ID)
  w$..blocked_handlers <- unique(c(w$..blocked_handlers, ID))
}

unblockHandler <- function(obj, ID=NULL,...) UseMethod("unblockHandler")
unblockHandler.gWidget <- function(obj, ID=NULL, ...) {
  w <- obj$toplevel  
  if(is.null(ID)) {
    w$..blocked_handlers <- c()
  } else {
    ID <- as.numeric(ID)
    if(ID %in% w$..blocked_handlers)
      w$..blocked_handlers <- unique(setdiff(w$..blocked_handlers, ID))
  }
}



##################################################
## Javascript handlers -- not submit handlers for buttons etc.


## Handler code

## code to run a handler.
## This must be exported.
## Called from www page
## worry about scope!! XXX -- doesn't seem to work

## XXX replaces by runHandler in proto gwindow object
runHandler <- function(obj, id, context) {
  obj <- get(obj, envir = .GlobalEnv)
  lst <- obj$jscriptHandlers[[as.numeric(id)]]
  h <- list(obj=lst$obj, action = lst$action)
  if(!missing(context) && context != "") {
    ## context is a list passed in through a JSON object converted into a list
    ## we pass this list into h object as context
    h$context <- context
  }
  return(lst$handler(h))
}



##' the Javascript queue
##'
##' When a handler is called there are two parts: one internal to the
##' R session, one to create javascript to output to the browser. This
##' queue stores the latter. Depending on how gWidgetsWWW is run it
##' either returns a string (help server) qor cats out a string
##' (RApache)

##' Add string to current queue
##' @param . EXTWidget
##' @param x code from xxxJS (setValueJS, ...), String() class
##' @return void
EXTWidget$addJSQueue <- function(., x) {
  parent <- .$toplevel
  curQueue <- parent$JSQueue
  if(length(curQueue) == 0)
    curQueue <- x
  else
    curQueue <- c(curQueue, x)
  parent$JSQueue <- curQueue
}

##' run the queue. Called by gwindow::runHandler, and by hanging event (ala r-studio)
##'
##' @param . EXTWidget
##' @return clears queue, then returns string with handler's output pasted together
EXTWidget$runJSQueue <- function(.) {
  parent <- .$toplevel
  curQueue <- parent$JSQueue
  if(is.null(curQueue))
    out <- ""
  else
    out <- paste(curQueue, collapse="\n")
  
  parent$JSQueue <- character(0)        # clear queue
  return(out)
}

  


## code to write out JS for the handlers on a object
## tricky part is that handlers must also be called for
## transport signals

## can override this per widget if desired!
## These were cherry picked from the Ext docs. Some may be missing,
## many are only available through the addHandler() method.

EXTWidget$handlerArgumentsList <-
  list(afteredit = "e",                 # for gdf cell editing
       blur="w",                        # w = "this"
       bodyresize = "w, width, height",
       bodyscroll = "scrollLeft, scrollRight",
       cellcontextmenu = "w, rowIndex, cellIndex, e",
       cellclick = "w, rowIndex, columnIndex, e", # grid
       celldblclick = "w, rowIndex, columnIndex, e", # grid
       cellmousedown = "w, rowIndex, columnIndex, e", # grid
       change="w, newValue, oldValue", beforechange = "w, newValue, oldValue",
       check = "w, checked",
       collapse = "w",                  # combobox
       columnmove = "oldIndex, newIndex",
       columnresize = "columnIndex, newSize",
       dblclick = "e",                  # grid -- not celldblclick
       destroy="w", beforedestroy = "w",
       disable="w",
       drag = "w, e", dragend = "w,e", dragstart = "w,e",
       enable = "w",
       expand = "w",                    # combobox
       fileselected = "w, s",               # FileUploadField
       focus = "w",
       headerclick = "w, columnIndex, e", # grid
       headercontextmenu = "w, columnIndex, e", # grid
       headerdblclick = "w, columnIndex, e", # grid
       headermousedown = "w, columnIndex, e", # grid       
       hide = "w", beforehide = "w",
       invalid = "w",
       keydown = "w,e",                 # e Ext.EventObject
       keypress = "w,e",
       keyup = "w,e",
       mousedown = "e",
       mouseover = "e", 
       mousemove = "e", 
       move = "w, x, y",
       render = "w", beforerender = "w",
       resize = "w, adjWidth, adjHeight, rawWidth, rawHeight",
       rowclick = "w, rowIndex, e", # grid
       rowcontextmenu = "w, rowIndex, e", # grid
       rowdblclick = "w, rowIndex, e", # grid
       rowmousedown = "w, rowIndex, e", # grid       
       select = "w,record,index", beforeselect = "w, record, index",
       selectionchange = "selModel",    # gcheckboxgrouptable
       show = "w", beforeshow = "w", 
       specialkey = "w, e",
       toggle = "w, value",             # gtogglebutton
       valid = "w")

##' process the list above allowing for local overrides                                       
EXTWidget$handlerArguments <- function(.,signal) {
  out <- .$handlerArgumentsList
  if(exists("..handlerArgumentsList", envir=., inherits = FALSE)) {
    for(i in names(.$..handlerlArgumentsList))
      out[[i]] <- .$..handlerlArgumentsList[[i]]
  }
  val <- ifelse(is.null(out[[signal]]), "", out[[signal]])
  return(val)
}

##' Write the handler part of the call function(...) (define_value;runHandlerJS(...))
##'
##' No trailing ; after function(...) {...}
##' @param . self
##' @param signal signal for handler to be called on.
##' @param handler handler list, prepared elsewhere
EXTWidget$writeHandlerFunction <- function(., signal, handler) {

  out <- String() +
    sprintf("function(%s) {runHandlerJS(%s%s);}",
            .$handlerArguments(signal),
            handler$handlerID,
            ifelse(!is.null(handler$handlerExtraParameters),
                   paste(",", handler$handlerExtraParameters, sep=""),
                   "")
            )

  out <- out + "\n"
  ## out <- String() +
  ##   'function(' + .$handlerArguments(signal) + ') {'  +
  ##     'runHandlerJS(' + handler$handlerID
  ## if(!is.null(handler$handlerExtraParameters)) {
  ##   out <- out + "," + handler$handlerExtraParameters
  ## }
  ## out <- out + ');' +  '}' + '\n'
  return(out)
}





## write out a single handler passed as a list
## the special case signal=idle is different
EXTWidget$writeHandlerJS <- function(.,signal="",handler=NULL) {
  if(is.null(signal))                   # errors?
    return()
  out <- String()
  if(signal == "idle") {
    out <- out +
      sprintf("setInterval(function() {runHandlerJS(%s%s)}, %s)\n",
              handler$handlerID,
              ifelse(!is.null(handler$handlerExtraParameters),
                     paste(",", handler$handlerExtraParameters, sep=""),
                     ""),
              handler$handlerArguments                     # duration
              )

    ## out <- out +
    ##   'setInterval(function() {' +
    ##     'runHandlerJS(' + handler$handlerID
    ## if(!is.null(handler$handlerExtraParameters))
    ##   out <- out + "," + handler$handlerExtraParameters
    ## out <- out +
    ##   ');' +
    ##     '},' + handler$handlerArguments + ');' + '\n'
  } else {
    
    ## write out transport if necessary
    ## XXX code to pass values createDelegate ....
    if(!is.null(.$transportSignal) && signal %in% .$transportSignal) {
      out <- out +
        sprintf("%s.on('%s', function(%s) {%s}, this, {delay:1, buffer:1, single:false});\n",
                .$asCharacter(),
                signal,
                .$handlerArguments(signal),
                .$writeTransport(signal=signal))
        
        ## 'o' + .$ID + '.on(' +
        ##   ## XXX transport args needs to be siganl dependent!!
        ##   shQuote(signal) + ', ' +
        ##     'function(' + .$handlerArguments(signal) + ') {\n' +
        ##       .$writeTransport(signal = signal) +
        ##         '}' +
        ##           ',this, {delay:1,buffer:1, single:false});' + '\n'
    }

    
    ## write out handler if needed
    if(!is.null(handler)) {
      out <- out +
        sprintf("%s.on('%s', %s, this, {delay:100, buffer:100, single:false});\n",
                .$asCharacter(),
                signal,
                .$writeHandlerFunction(signal=signal, handler=handler) ## includes function() {}
                )
##         'o' + .$ID + '.on(' +
##           ## XXX transport args needs to be siganl dependent!!
##           shQuote(signal) + ',' +
##             .$writeHandlerFunction(signal=signal, handler=handler) +
## ',this, {delay:100,buffer:100, single:false});' + '\n'

    }
#  
#    out <- out +
#      '},this, {delay:100,buffer:100, single:false});' + '\n'
  }

  return(out)
}

##' Loops to write out all handlers
EXTWidget$writeHandlersJS <- function(.) {
  if(exists("..handlers", envir=., inherits=FALSE))
    allHandlers <- .$..handlers
  else
    allHandlers <- list()

  ## get all signals
  signals <- c()
  if(!is.null(.$transportSignal))
    signals <- .$transportSignal
  if(length(allHandlers) > 0)
    signals <- union(signals, names(allHandlers))

  if(length(signals) == 0) return(String(""))     # nothing to do
  
  out <- String()
  for(sig in signals) {
    out <- out + .$writeHandlerJS(sig, allHandlers[[sig]])
  }

  return(out)
}
      
## handlerExtraParameters is NULL or a JSON string to evaluate to a list
## it is passed into the handler in the $context component
EXTWidget$addHandler <- function(., signal, handler, action=NULL,
                                 handlerArguments="w",
                                 handlerExtraParameters=NULL,
                                 handlerValue = NULL,
                                 ...
                                 ) {

  lst <- list(obj = .,
              signal=signal,
              handler=handler,
              action=action,
              scope = parent.frame(),
              handlerArguments = handlerArguments, # eg "widget,evt"
              handlerExtraParameters = handlerExtraParameters, # eg ", Ext.util.JSON.encode({keypress:evt.getKey()})"
              handlerValue = handlerValue,                      # eg "var value = rowIndex -1" for GridPanel instances
              args = list(...)
              )

  ## we put handlers into parent widget
  ## regardless of whether we have shown the object or not
  parent <- .$toplevel
  curHandlers <- parent$jscriptHandlers
  n <- length(curHandlers)
  lst$handlerID <- n + 1
  if(n > 0) {
    parent$jscriptHandlers[[n+1]] <- lst
  } else {
    parent$jscriptHandlers <- list(lst)
  }
  
  ## add handler to list of handlers in object
  if(!is.null(signal)) {
    if(exists("..handlers", envir=., inherits = FALSE))
      curHandlers <- .$..handlers
    else
      curHandlers <- list()
    ## add handler
    curHandlers[[signal]] <- lst
    ## add back to object
    .$..handlers <- curHandlers
  }

  
  ## there are, as with other cases, two states
  ## if widget is not shown, then we
  ## a) add handler to parent so that it cna be written out.
  ## b) the actual handler code will be written out in gwindow
  ## If the widget is shown, then we need to
  ## a) store the handler in the global variable w$titlename
  ## b) write out the Javascript to
  ##    1) set the transport function (if necessary)
  ##    2) write the handler

  if(exists("..shown", envir=., inherits = FALSE)) {
    ## need to write out the JS to show the handler
    ## cat(.$writeHandlerJS(signal, lst))          # a single handler
    out <- .$writeHandlerJS(signal, lst)
    .$addJSQueue(out)
    ##cat(out)
  }
    
  ## we return the ID
  return(invisible(lst$handlerID))
}

"addHandler" <- function(obj,signal, handler, action=NULL,...)
  UseMethod("addHandler")
addHandler.gWidget <- function(obj,signal,handler, action=NULL,...)
  obj$addHandler(signal, handler, action,...)


## instances
## addHandlerBlur
EXTWidget$addHandlerBlur <- function(., handler, action=NULL) {
  .$addHandler(signal="blur",handler, action)
 }

"addHandlerBlur" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerBlur")
addHandlerBlur.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerBlur(handler, action)


## addHandlerChanged
EXTWidget$addHandlerChanged <- function(., handler, action=NULL) {
  .$addHandler(signal="change",handler, action)
}

"addHandlerChanged" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerChanged")
addHandlerChanged.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerChanged(handler, action)

## addHandlerClicked
EXTWidget$addHandlerClicked <- function(., handler, action=NULL) {
  .$addHandler(signal="click",handler, action)
}

"addHandlerClicked" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerClicked")
addHandlerClicked.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerClicked(handler, action)

## addHandlerDoubleclick
EXTWidget$addHandlerDoubleclick <- function(., handler, action=NULL) {
  .$addHandler(signal="dblclick",handler, action)
}

"addHandlerDoubleclick" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerDoubleclick")
addHandlerDoubleclick.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerDoubleclick(handler, action)


## addHandlerMouseclick
EXTWidget$addHandlerMouseclick <- function(., handler, action=NULL) {
  .$addHandler(signal="mousedown",handler, action,
               handlerArguments="e",
               handlerExtraParameters = "Ext.util.JSON.encode({xy:[e.layerX,e.layerY]})"
               )
  
}

"addHandlerMouseclick" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerMouseclick")
addHandlerMouseclick.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerMouseclick(handler, action)

## addHandlerKeystroke
## key passed in is ASCII code.
EXTWidget$addHandlerKeystroke <- function(., handler, action=NULL,...) {
  .$addHandler(signal="keydown",handler, action,
               handlerArguments="b,e",
               handlerExtraParameters = "Ext.util.JSON.encode({key: e.getKey()})",
               ...
               )
}

## This handler shows how we can pass in extra information to the handler and then
## use this by writing a custom writeHandlerFunction.
## For EXTComponentText we do so to handle the key events as doing so by calling back into
## R each time is too expensive.
## Here *if* we pass either key (which should be of the form "A" or "a", but likely won't work unless
## it is of the form "e.ENTER, or
## BACKSPACE:8,TAB:9,NUM_CENTER:12,ENTER:13,RETURN:13,SHIFT:16,CTRL:17,CONTROL:17,ALT:18,PAUSE:19,CAPS_LOCK:20,ESC:27,SPACE:32,PAGE_UP:33,PAGEUP:33,PAGE_DOWN:34,PAGEDOWN:34,END:35,HOME:36,LEFT:37,UP:38,RIGHT:39,DOWN:40,PRINT_SCREEN:44,INSERT:45,DELETE:46,ZERO:48,ONE:49,TWO:50,THREE:51,FOUR:52,FIVE:53,SIX:54,SEVEN:55,EIGHT:56,NINE:57,A:65,B:66,C:67,D:68,E:69,F:70,G:71,H:72,I:73,J:74,K:75,L:76,M:77,N:78,O:79,P:80,Q:81,R:82,S:83,T:84,U:85,V:86,W:87,X:88,Y:89,Z:90,CONTEXT_MENU:93,NUM_ZERO:96,NUM_ONE:97,NUM_TWO:98,NUM_THREE:99,NUM_FOUR:100,NUM_FIVE:101,NUM_SIX:102,NUM_SEVEN:103,NUM_EIGHT:104,NUM_NINE:105,NUM_MULTIPLY:106,NUM_PLUS:107,NUM_MINUS)
## or charCode which can be a numeric value, eg ENTER = 13, then before running the handler, in javasscript land a check will be made. It seems that Ctrl+Enter, say, is not detected as such.


"addHandlerKeystroke" <- function(obj, handler, action=NULL,key=NULL, charCode=NULL, ...)
   UseMethod("addHandlerKeystroke")
 addHandlerKeystroke.gWidget <- function(obj, handler, action=NULL, key=NULL, charCode=NULL, ...)
   obj$addHandlerKeystroke(handler, action, key=key, charCode=charCode, ...)

 ## addHandlerSelect
 EXTWidget$addHandlerSelect <- function(., handler, action=NULL) {
   .$addHandler(signal="onselect",handler, action)
 }

 "addHandlerSelect" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerSelect")
 addHandlerSelect.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerSelect(handler, action)


 ## addHandlerDestroy
 EXTWidget$addHandlerDestroy <- function(., handler, action=NULL) {
   .$addHandler(signal="onunload",handler, action)
 }

 "addHandlerDestroy" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerDestroy")
 addHandlerDestroy.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerDestroy(handler, action)

 ## addHandlerExposed
 EXTWidget$addHandlerExposed <- function(., handler, action=NULL) {
   .$addHandler(signal="onLoad",handler, action)
 }

 "addHandlerExposed" <- function(obj, handler, action=NULL)
   UseMethod("addHandlerExposed")
 addHandlerExposed.gWidget <- function(obj,handler, action=NULL)
   obj$addHandlerExposed(handler, action)

 ## addHandlerMouseMotion
 EXTWidget$addHandlerMouseMotion <- function(., handler, action=NULL) {
   .$addHandler(signal="mousemove",handler, action,
                handlerArguments="e",
                handlerExtraParameters = "EXT.util.JSON.encode({xy:[e.layerX, e.layerY]})"
                )
 }

"addHandlerMouseMotion" <- function(obj, handler, action=NULL)
  UseMethod("addHandlerMouseMotion")
addHandlerMouseMotion.gWidget <- function(obj,handler, action=NULL)
  obj$addHandlerMouseMotion(handler, action)


## not implemented
"addHandlerIdle" <- function(obj, handler = NULL, action = NULL, interval = 1000,   ...)
  UseMethod("addHandlerIdle")
addHandlerIdle.gWidget <- function(obj, handler=NULL, action=NULL,interval = 1000, ...)
  obj$addHandlerIdle(handler, action,interval,...)

EXTWidget$addHandlerIdle <- function(., handler=NULL, action=NULL, interval=1000, ...) {
  ## setInterval(expression, interval) is the javascript to call here
  ## Need to trap this signal, as it doesn't fit the typical pattern
  .$addHandler(signal="idle", handler = handler, action=action, handlerArguments=interval,...)
}




