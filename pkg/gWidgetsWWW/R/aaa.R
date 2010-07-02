require(proto, quietly=TRUE)
#require(filehash, quietly=TRUE)
#require(digest, quietly=TRUE)

## Three main classes
## EXTWidget and its subclasses EXTComponent, EXTContainer


## The double dot is meant to indicate an instance variable/method as
## opposed to a "class" variable/method. It seems that proto does not
## propogate the double dot -- that is

## > a = proto(..test = TRUE, test = TRUE, new = function(.) .$proto())
## > b = a$new()
## > b$test
## [1] TRUE
## > b$..test
## Error in get(x, env = this, inherits = inh) : 
##   variable "..test" was not found
##
## so we check for ..test using exists and inherits = FALSE

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
        file = "",                      # for "cat"; in parent of widget
        ExtConstructor = NULL,          # eg. Ext.Button
        ExtCfgOptions = NULL,           # config options -- fn returns a list
        ..ExtCfgOptions = NULL,         # additional options per instance
        getValueJSMethod = NULL,        # name of method, eg. "getValue"
        setValueJSMethod = NULL,        # name of method, eg. "setValue"
        coerce.with = NULL,             # coerce FUN or string
        transportSignal = NULL          # possibly a vector
        )



## Some configuration

### methods

## used to display to STDOUT or a file for debugging
## XXX Should we use .$file <- stdout()???
## this isn't working for dialogs
EXTWidget$Cat <- function(.,...) {
  cat(...,"\n", file=.$file, append=TRUE)
}
EXTWidget$test <- function(.,...) .$Cat("alert('test message');")

## Cat either a string or function
EXTWidget$showPart <- function(.,part) {
  ## part may be property of function. This checks
  if(!is.null(part))
    if(is.function(part))
      .$Cat(part())
    else
      .$Cat(part)
}

## instead of pasting "o" in front of ID
## we have Ext Object "o" + ID and DOM object "ID" to consider
## XXX This is misnamed (toString?)
EXTWidget$asCharacter <- function(.) {String('o') +  .$ID}

## simple function to call an Ext method on the corresponding object
EXTWidget$callExtMethod <- function(., methodname, args) {
  if(missing(args))
    args <- '();'
  else
    args <- String('(') + args + ');'
  out <- String() +
    .$asCharacter() + "." + methodname + args
  return(out)
}




## We have both S3 methods and their proto counterparts

## Here we have getValue, setValue for svalue, svalue<-
## svalue, svalue<-
EXTWidget$getValue <- function(., index=NULL,drop=NULL, ...) {
  if(exists("..shown",envir=.,inherits=FALSE)) {
     ## get from widget ID
     out <- try(get(.$ID,envir=.$toplevel),silent=TRUE) ## XXX work in index here?
     if(inherits(out,"try-error")) {
       out <- .$..data
     } else {
       .$..data <- out                  # update data
     }
   } else {
     if(is.null(index) || !index) {
       out <- .$..data
     } else {
       values <- .$getValues()
       if(is.data.frame(values))
         values <- values[,1, drop=TRUE]
       out <- which(.$..data %in% values)
    }
   }
  out <- .$coerceValues(out)
  return(out)
}

## have we shown the widget? if so, we set in document too
## We need to also assign to .$ID in . as otherwise
## we don't get the getValue right
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
    if(.$ID != "")
      assign(.$ID, newVal, envir=.$toplevel)
  }
  ## now process if shown
  if(exists("..shown",envir=., inherits=FALSE)) 
    cat(.$setValueJS(index=index,...),file=stdout())

 }
## create javascript code to write Javascript to set
## the value of a widget
## properties setValueMethod, 
EXTWidget$setValueJSAttribute = "innerHTML"

## this uses the DOM value -- not the Ext widget. EXTComponent
## overrides this.
EXTWidget$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

  
  ## default method to set the value using setValue
  value <- .$..data                     # don't use svalue here
  
  
  out <- String('var widget = ') +
    'EXT.get(' + shQuote(.$ID) + ');' +
      'widget.' + .$setValueJSAttribute + '= ' + shQuote(value) + ';\n'
  
  return(out)                              # to browser, not file
}


## function to coerce values using either coerce.with or ..coerce.with
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

EXTWidget$getValues <- function(., ...) .$..values
EXTWidget$setValues <- function(.,i,j,...,value) {
  ## XXX Need to include i,j
  .$..values <- value
  if(exists("..shown",envir=., inherits=FALSE))
    cat(.$setValuesJS(...), file=stdout())
}
EXTWidget$setValuesJS <- function(.,...) {
  if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)  
  return("")                               # cat javascript to set values
}

## length, dim -- issue with recursion if given full name
length.gWidget <- function(x) {. = x; .$.length()}
EXTWidget$.length <- function(.) {vals <- .$..values; length(vals)}
dim.gWidget <- function(x) {. = x; .$.dim()}
EXTWidget$.dim <- function(.) {vals <- .$..values; dim(vals)}

## names, names<-
EXTWidget$getNames <- function(.) .$names
EXTWidget$setNames <- function(.,value) {
  .$..names <- value
  if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$setNamesJS(), file=stdout())
  }
}
EXTWidget$setNamesJS <- function(.) {}    # set names


## visible<-
EXTWidget$getVisible <- function(.) return(.$..visible )
EXTWidget$setVisible <- function(.,value) {
  .$..visible <- as.logical(value)
  if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$setVisibleJS(), file=stdout())
  }
}
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
EXTWidget$getEnabled <- function(.) return(.$..enabled )
EXTWidget$setEnabled <- function(.,value) {
  .$..enabled <- as.logical(value)
  if(exists("..shown",envir=., inherits=FALSE)) 
    cat(.$setEnabledJS(), file=stdout())
}
EXTWidget$setEnabledJS <- function(.) {
  if(exists("..enabled", envir=., inherits=FALSE))
    value <- as.logical(.$..enabled)
  else
    value <- TRUE

  ## which method
  method <- ifelse(value, "enable", "disable")

  out <- String() +
    'o' + .$ID + '.' + method + '();' + '\n'
  
##   ## uses DOM
##   out <- String() +
##     'var widget = ' +
##       'Ext.get(' + shQuote(.$ID) + ');\n' +
##        'widget.' + method + ';\n'
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
  

 ## set size.
 ## calls setStyle
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

EXTWidget$ExtStdCfgOptions <- function(.) {
  out <- list(
              "renderTo"=String("Ext.getBody()"),
              "id"=.$ID
              )
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


       
## method to coerce ExtCfgOptions into a string
## ExtCfgOptions is a list. The names are passed as keys and the values
## are assigned.
## Object Literals in Ext are like passing lists through the ... argument
## characters are quoted, String()s are not. Sometimes
## as.character(String() + "...") is useful.
## This function recurses when the value is a list
## method coerceToJSString is in common.R

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

  res <- paste(names(out), out, sep=":", collapse=",")
  if(doBraces)
    res <- String('{') + res + '}'

  return(res)
}

## Basic template for a EXT widget.
## There are several stages.
## header and footer are used to wrap the object if desired

## The header and footer are blank
EXTWidget$header <- EXTWidget$footer <- EXTWidget$separator <- function(.) return("")

EXTWidget$writeConstructor <- function(.) {
  out <- String() +
### var creates a *local* variable -- issues with safari here
###    'var o' + .$ID +
    'o' + .$ID +
      ' = new ' +.$ExtConstructor + '(\n' +
        .$mapRtoObjectLiteral() +
          ');\n'
  ## write out x-hidden unless requested not to.
  ## x-hidden causes the widget not to display until added to parent
  top <- .$toplevel
  if(!is.null(top)) {
    if(!exists("..shown", envir=top, inherits=FALSE) ||
       (exists("..shown", envir=top, inherits=FALSE)  && !top$..shown)) {
      if(!exists("no.x.hidden",envir=., inherits=FALSE))
        out <- out +
          'Ext.get(' + shQuote(.$ID) +').addClass("x-hidden");\n'
    }
  }

  ## add in at the end 
  if(exists("..writeConstructor", envir=., inherits=FALSE)) {
    out <- out + .$..writeConstructor()
  }

  return(out)
}

## For controls whose value may be changed by the GUI, we write out changes
## immediately back to R so that R handlers will be aware of the changes. We
## call this transport.

## code to write out transport function
## called in writeHandlers
EXTWidget$transportValue <- function(.,...) {
  out <- String() +
    'var value = o' + .$ID + '.' +
      .$getValueJSMethod + '();\n'
  return(out)
}
EXTWidget$transportFUN <- function(.) {
  out <- String() +
    '_transportToR(' + shQuote(.$ID) +', Ext.util.JSON.encode({value:value}));'
  return(out)
}

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

## writes tooltip. Tooltips are added with tooltip<- function
## value can be a URL (isURL == TRUE) or a string or a character vector which
## gets pasted together to be a string
EXTWidget$tooltipWidth <- 200
EXTWidget$tooltipAutoHide <- TRUE # override to 
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

## show object, including catting it
## called by Show
EXTWidget$show <- function(.) {
  out <- String("\n") +
    .$writeConstructor() +
      .$setStyleJS(styles=.$style) +
          .$writeTooltip() +
            .$writeHandlersJS()           # includes transport
  
  .$Cat(out)
}


##################################################
## Some "subclasses" of EXTWidget defined below

## EXT Component -- for controls, etc

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

EXTComponent$Show <- function(.,...) {        # wraps Show
  ## add in any instance specific scripts
  ## component specific scripts written out once in gwindow
  if(exists("..scripts",envir=., inherits=FALSE)) 
    .$showPart(.$..scripts)
  
  ## make an object with method?
  if(exists("..header",envir=.,inherits=FALSE))  .$showPart(.$..header)
  .$showPart(.$header)

  
  .$show()                      # show self
  .$..shown <- TRUE             # set shown (rendered)
  
  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer)
  .$showPart(.$footer)
}



### Methods have two parts
### * one for first showing (sets value in R)
### * one after shown -- returns string with javascript

EXTComponent$setValueJSAttribute <- "value"
EXTComponent$setValueJSMethod = "setValue"  # oID.method()
EXTComponent$setValueJS <- function(.,...) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
   ## default method to set the value using setValue
   value <- .$..data                     # don't use svalue here


   ## use Ext object and a method directly -- no DOM
   out <- String() +
     'o' + .$ID +'.' + .$setValueJSMethod +
       '(' + toJS(.$..data) + ');' + '\n'
   
   return(out)                              # to browser, not file
 }

 EXTComponent$setValuesJS <- function(.,...) {
   if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)  
   return("")
 }


EXTComponent$setNamesJS <- function(.) {}    # set names
## get from EXTWidget
## EXTComponent$setVisibleJS <- function(.) {
##   if(exists("..setVisibleJS", envir=., inherits=FALSE))
##     .$..setVisibleJS()
  
##   if(.$..visible)
##     method = "hide"
##   else
##     method = "show"
##   .$callExtMethod(method)
## }


## Different components

EXTComponentResizable <- EXTComponent$new()

## footer adds in a resizable conainer -- notw orking?
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

##
## We have gedit, gtext with key events, that are a bit different for handlers
## as we want to intercept the event with javascript
EXTComponentText <- EXTComponent$new()

EXTComponentText$writeHandlerFunction <- function(., signal, handler) {
   out <- String() +
    'function(' + .$handlerArguments(signal) + ') {'

   tmp <- String() +
     'runHandlerJS(' + handler$handlerID
   if(!is.null(handler$handlerExtraParameters)) {
     tmp <- tmp + "," + handler$handlerExtraParameters
   }
   tmp <- tmp + ');'

   ## need to do transport
   
   tmp1 <- String() +
     "var value = escape("+ "o" + .$ID + ".getValue());" + 
       "_transportToR('" + .$ID + "', Ext.util.JSON.encode({value:value}));" + "\n"

   ## wrap inside conditional
   if(!is.null(handler$args$key)) {
     key <- handler$args$key
     out <- out + "if(e.getCharCode() ==" +
       ifelse(is.numeric(key) || nchar(key) == 1, shQuote(key),key) +
       ") {" +
         tmp1 +
           tmp +
           "};"
   } else if(!is.null(handler$args$charCode)) {
     key <- handler$args$charCode
     out <- out + "if(e.getCharCode() ==" +
       ifelse( nchar(key) == 1, shQuote(key),key) + ") {" +
         tmp1 + tmp +
         "};"
   } else {
     out <- out + tmp
   }
   ## close up
   out <- out + '}' + '\n'
  return(out)
}


##
##
###########

## Container Trait. Main methods
## are:
## newID -- ID for a widget generated when added to a container.
##   This also copies over stuff to
## add -- adds to list for later rendering
## Show -- to show object -- loops over children

EXTContainer <- EXTWidget$new(children = list(),
                              width = "auto",
                              height = "auto",
                              makeItemsFixedItems = "" # to make items[]
                              )
## each object gets an ID.
EXTContainer$newID <- function(.) {
  IDS <- .$toplevel$..IDS
  n <- length(IDS)
  newID <- paste("gWidgetID",n+1,sep="")
  .$toplevel$..IDS <- c(IDS,newID)
  return(newID)
}
## add for a container does several things:
## * set toplevel for each child
## * add css to toplevel if applicable
## * add javascript to toplevel if applicable
## * set object into child
## ... is not used XXX should fix this.
EXTContainer$add <- function(.,child,...) {

   ## add an ID
   child$ID <- .$newID()

   ## add parent to child for traversal
   child$parent = .
   child$toplevel = .$toplevel         # pass in toplevel window

   ## pass along parent properties
   child$file <- .$file
   child$titlename <- .$titlename

   ## Move scripts, css to toplevel
   if(!is.null(child$css)) {
     css <- .$toplevel$css
     if(is.function(child$css))
       css[[class(child)[1]]] <-
         list(obj = child, FUN = get("css",child))
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

       
       if(exists("..shown", envir=.$toplevel, inherits=FALSE) && .$toplevel$..shown) {
         ## need to cat this script out now,
         ## This prevents things being defined in subwindows
         ## for first time
         i <- scripts[[class(child)[1]]]
         if(is.list(i))
           cat(i$FUN(i$obj), file=stdout())
         else if(is.character(i))
           cat(i, file=stdout())
        }
     }
   }
   

   
   ## add to children
   lst <- .$children
   .$children <- c(lst, child)

   if(exists("..shown",envir=., inherits=FALSE)) {
     if(!inherits(child,"gSubwindow")) {
       child$Show()
       cat(.$addJS(child))
     }
   }
   
 }
## write out JS to add child after things have been shown
## this is likely not perfect
EXTContainer$addJS <- function(.,child) {
  out <- String() +
    .$asCharacter() + '.add(' + child$asCharacter() + ');' +
      .$asCharacter() + '.doLayout();'

  return(out)
}

## remove a widget
EXTContainer$delete <- function(., widget) {
  ## remove widget from obj
  if(exists("..shown", envir=., inherits=FALSE)) {
    cat(.$deleteJS(widget))
  }
}

EXTContainer$deleteJS <- function(., widget) {
  out <- String() +
    .$asCharacter() + '.remove(' + widget$asCharacter() + ');'
  cat(out)
}
      

## for containers width and height are properties, not in .style
EXTContainer$setSize <- function(., value) {
  .$width <- value[1]
  if(length(value) > 1)
    .$height <- value[2]
}
EXTContainer$getSize <- function(.) c(width=.$width,height=.$height)

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



## Containers have children to show too.
## also a separator is possible to display between the children,
## although this should go
EXTContainer$Show <- function(.) {
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
      .$Cat(out)
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
    .$Cat(out)
  }


  ## now show container
  if(exists("..header",envir=.,inherits=FALSE))  .$showPart(.$..header)
  .$showPart(.$header)

  
  ## write out actions if present
  if(exists("..actions", envir = ., inherits = FALSE)) {
    if(length(.$..actions) > 0) {
      for(i in .$..actions) {
        i$Show();
        i$..shown <- TRUE
      }
    }
  }

  

  children <- .$children
  if((n <- length(children)) > 0) {
    for(i in 1:n) {
      children[[i]]$Show()              # Show children
      if(i < n) {
        if(exists("..separator",envir=.,inherits=FALSE))
          .$showPart(.$..separator)       # widget specific
        .$showPart(.$separator)
      }
    }
  }

  .$show()                      # show self
  .$..shown <- TRUE                     # set shown

  ## handlers ## gwindow only
  if(exists("..setHandlers",envir=.,inherits=FALSE)) # gwindow only
     .$showPart(.$..setHandlers)

  if(exists("..footer",envir=.,inherits=FALSE))  .$showPart(.$..footer)
  .$showPart(.$footer)

}

## items are how containers refer to their children

## this will be overridden more than likely
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

## overritde EXTWidget$show, as
## unlike a EXTComponent, here we need to add in the items too
EXTContainer$show <- function(.) {
  out <- String() +
    'o' + .$ID + '= new ' + .$ExtConstructor + '({' + '\n' +
        .$mapRtoObjectLiteral(doBraces=FALSE) +
          ',' + '\n' +
            'items:[' +.$makeItems() +
              ']\n' + '});' 
  .$Cat(out)
}

##################################################
##
## Some widget have a data store associated with them
## eg. gcombobox, gtable, gdf 

EXTStore <- proto()
EXTStore$new <- function(.) {
  obj <- .$proto()
  class(obj) <- c("gWidgetStore",class(obj))
  invisible(obj)
}
## properties
## the data frame (or vector)
EXTStore$ID <- NULL                      # get from newID
EXTStore$data <- NULL
EXTStore$chosenCol <- NULL               # selected column

## methods
## coerce val to a JS array
EXTStore$setData <- function(.,d) .$data <- d
EXTStore$getData <- function(.) .$data
EXTStore$setChosenCol <- function(.,value).$chosenCol <- value
EXTStore$getChosenCol <- function(.).$chosenCol

EXTStore$asJSArray <- function(.,val) {
  if(missing(val)) val <- .$data
  toJSArray(val)
}
EXTStore$asCharacter <- function(.) String('o') + .$ID + 'store'
EXTStore$displayField <- function(.) .$chosenCol

EXTStore$fieldNames <- function(.) {names(.$data)}
## for combo tihs is just an array, 
## for a grid object it is more work
EXTStore$makeFields <- function(.) {
  .$asJSArray(.$fieldNames())
}
EXTStore$show <- function(.) {
  out <- .$asCharacter() + '= new Ext.data.ArrayStore({' +
    'fields:  ' + .$makeFields() + ',' + '\n' +
      'data: ' + .$asJSArray() +
        '\n' + '});' + '\n'
  return(out)
}

## replace the store with this data frame
EXTStore$replaceStore <- function(., data) {
  if(!missing(data)) .$data <- data
  out <- String() +
    .$asCharacter() + '.removeAll();' +
      .$asCharacter() + '.loadData(' +
        .$asJSArray() + ');'

  return(out)
}
## XXX need more granular approach


## extend Component to handle a data store
EXTComponentWithStore <- EXTComponent$new()

## additional properties

## store -- holds an EXTStore instance
EXTComponentWithStore$..store <- NULL

## methods
EXTComponentWithStore$getValues <- function(., ...) {
  tmp <- .$..store$data
  if(names(tmp)[1] == "..index")
    tmp <- tmp[,-1]
  tmp
}
EXTComponentWithStore$getLength <- function(.)
  length(.$getValues())
EXTComponentWithStore$getNames <- function(.)
  names(.$getValues())
## XXX names<- not defined
EXTComponentWithStore$setValues <- function(.,i,j,...,value) {
  ## XXX need to include i,j stuff
  .$..store$data <- value
  if(exists("..shown",envir=., inherits=FALSE))
    cat(.$setValuesJS(...), file=stdout())
}
EXTComponentWithStore$setValuesJS <- function(.) {
  if(exists("..setValuesJS", envir=., inherits=FALSE)) .$..setValuesJS(...)
  
  out <- String() +
    .$..store$asCharacter() + '.removeAll();' +
      .$..store$asCharacter() + '.loadData(' +
        .$asJSArray(.$..store$data) +');'

  return(out)
}


## turn an object into an array
EXTComponentWithStore$asJSArray <- function(.,...) {
  .$..store$asJSArray(...)
}
## show needs to show store and component
EXTComponentWithStore$show <- function(.) {
  .$Cat(.$..store$show())
  get("show",EXTComponent)(.)       # call up
}
## get name of store to paste into JS code
## This made adding the widget not work! replaced
#EXTComponentWithStore$asCharacter <- function(.) {
#
#  .$..store$asCharacter()               # call into store
#}


### Some widgets render better in a panel
## This overrides the writeConstructor method to show the object
## in ExtStdCfgOptions use an xtype and override renderTo with NULL
## see gcheckbox for an example
EXTComponentInPanel <- EXTComponent$new()
EXTComponentInPanel$getItemID <- function(.) String(.$ID) + 'item'
EXTComponentInPanel$writeConstructor <- function(.) {
  lst <- list(id = as.character(.$ID),
              xtype = "panel",
              layout = "fit",
              border = FALSE,
              hideBorders = TRUE,
              width = ifelse(exists("..width", ., inherits=FALSE),
                .$..width,"auto"),
              renderTo = String("Ext.getBody()"),
              items = String("[") + .$mapRtoObjectLiteral() + ']'
              )
  out <- String() +
    'o' + .$ID + 'panel = new Ext.Panel(' + # no var -- global
      .$mapRtoObjectLiteral(lst) +
        ');' + '\n'
  ## get component from first child object
  out <- out +
    'o' + .$ID + ' = ' +                # no var -- global
      'o' + .$ID + 'panel.getComponent("' + .$getItemID() + '");' + '\n'
  
  return(out)
}

## a component with items like gradio and gcheckboxgroup
## we use a panel and use the items to store the values
## the handlers need to be assigned to each 
EXTComponentWithItems <- EXTComponent$new()
EXTComponentWithItems$xtype <- ""       # eg "checkbox", "radio"
EXTComponentWithItems$itemname <- "item"
EXTComponentWithItems$ExtConstructor <- "Ext.Panel"
EXTComponentWithItems$no.x.hidden <- TRUE
EXTComponentWithItems$checked <- function(.,i) {
  ## return TRUE if checked o/w false
}
## make the items
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

## Modified from orignial to loop over all items
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
        'var widget = ' + .$asCharacter() + '.getComponent(' +
          as.character(i - 1) + ');' +
            'widget.on(' +
              ## XXX transport args needs to be siganl dependent!!
              shQuote(sig) + ',' +
                'function(' + .$handlerArguments(sig) + ') {\n'

      ## write out transport if necessary
      ## XXX code to pass values createDelegate ....
      if(!is.null(.$transportSignal) && sig %in% .$transportSignal) {
        out <- out + .$writeTransport(ext = shQuote(i), signal=sig) # ## pass this in
      }
      out <- out +'}' +
        ',this, {delay:1,buffer:1, single:false});' + '\n'
      
      ## write out handler if needed
      if(!is.null(allHandlers[[sig]])) {
        handler <- allHandlers[[sig]]
        out <- out +
          'var widget = ' + .$asCharacter() + '.getComponent(' + as.character(i - 1) + ');' +
            'widget.on(' +
            ## XXX transport args needs to be siganl dependent!!
            shQuote(sig) + ',' +
              'function(' + .$handlerArguments(sig) + ') {\n' +
                .$writeHandlerFunction(signal=sig, handler=handler) +
                  '\n'
        ##           'runHandlerJS(' + handler$handlerID  +
        ##             handler$handlerExtraParameters + ');' + '\n' +
        ##               'true;' + '\n'
        out <- out +
          '},this, {delay:100,buffer:100, single:false});' + '\n'
      }
    }
  }

  return(out)
}






##############################
## gwidget methods
svalue <- function(obj,index=NULL, drop=NULL,...) UseMethod("svalue")
svalue.gWidget <- function(obj,index=NULL, drop=NULL,...) {
  obj$getValue(index=index,drop=drop,...)
}

"svalue<-" <- function(obj,index=NULL, ...,value) UseMethod("svalue<-")
"svalue<-.gWidget" <- function(obj,index=NULL, ..., value) {
  obj$setValue(index=index,..., value=value)
  return(obj)
}

## add is used by gtext atleast. $Add implicitly used by contaienrs
"add" <- function(obj,value,...) UseMethod("add")
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
  if(missing(value)) value <- TRUE
  .$..enabled <- as.logical(value)

  if(exists("..shown", envir=., inherits=FALSE))
    cat(obj$setEnabledJS(), file=stdout())
  
  return(obj)
}


## dispose of widget. We simply hide it here
## no method until created
"dispose" <- function(obj,...) UseMethod("dispose")
"dispose.gWidget" <- function(obj,...) {
  . = obj

  if(exists("dispose", envir=.)) {
    .$dispose()
  } else if(exists("..shown",envir=., inherits=FALSE)) {
    cat(.$callExtMethod("hide"), file=stdout())
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
    cat(.$callExtMethod("focus",tolower(as.character(value))), file=stdout())

  return(obj)
}

## tag
tag <- function(obj, key, ...) UseMethod("tag")
tag.gWidget <- function(obj, key, ...)  {
  obj$.tag[[key]]
}
"tag<-" <- function(obj, key,...,value) UseMethod("tag<-")
"tag<-.gWidget" <- function(obj, key, ..., value) {
  l <- obj$.tag
  l[[key]] <- value
  obj$.tag <- l
  obj
}


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
"names<-.gWidget" <- function(x,value) {
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
    cat(.$setStyleJS(), file=stdout())
   }
  
  return(obj)
}

## we use print for gwindow and gsubwindow as a more natural alias
## than $Show() i.e. after storing objects into the toplevel window or
## its subwindos, we need to be able to show the contents to the
## brower. This is done with th eproto method $#Show(), which here is
## aliased to theprint method of the proto objects.
print.gWindow <- print.gSubwindow <- function(x,...) {
  . = x; .$Show()
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
       show = "w", beforeshow = "w", 
       specialkey = "w, e",
       valid = "w")

## process the list above allowing for local overrides                                       
EXTWidget$handlerArguments <- function(.,signal) {
  out <- .$handlerArgumentsList
  if(exists("..handlerArgumentsList", envir=., inherits = FALSE)) {
    for(i in names(.$..handlerlArgumentsList))
      out[[i]] <- .$..handlerlArgumentsList[[i]]
  }
  val <- ifelse(is.null(out[[signal]]), "", out[[signal]])
  return(val)
}

## abstract out function. Used with gradio
EXTWidget$writeHandlerFunction <- function(., signal, handler) {
  
  out <- String() +
    'function(' + .$handlerArguments(signal) + ') {'  +
      'runHandlerJS(' + handler$handlerID
  if(!is.null(handler$handlerExtraParameters)) {
    out <- out + "," + handler$handlerExtraParameters
  }
  out <- out + ');' +  '}' + '\n'
  return(out)
}

## write out a single handler passed as a list
## the special case signal=idle is different
EXTWidget$writeHandlerJS <- function(.,signal,handler=NULL) {
  out <- String()
  if(signal == "idle") {
    out <- out +
      'setInterval(function() {' +
        'runHandlerJS(' + handler$handlerID
    if(!is.null(handler$handlerExtraParameters))
      out <- out + "," + handler$handlerExtraParameters
    out <- out +
      ');' +
        '},' + handler$handlerArguments + ');' + '\n'
  } else {
    
    ## write out transport if necessary
    ## XXX code to pass values createDelegate ....
    if(!is.null(.$transportSignal) && signal %in% .$transportSignal) {
      out <- out +
        'o' + .$ID + '.on(' +
          ## XXX transport args needs to be siganl dependent!!
          shQuote(signal) + ', ' +
            'function(' + .$handlerArguments(signal) + ') {\n' +
              .$writeTransport(signal = signal) +
                '}' +
                  ',this, {delay:1,buffer:1, single:false});' + '\n'
    }

    
    ## write out handler if needed
    if(!is.null(handler)) {
      out <- out +
        'o' + .$ID + '.on(' +
          ## XXX transport args needs to be siganl dependent!!
          shQuote(signal) + ',' +
            .$writeHandlerFunction(signal=signal, handler=handler) +
',this, {delay:100,buffer:100, single:false});' + '\n'

    }
#  
#    out <- out +
#      '},this, {delay:100,buffer:100, single:false});' + '\n'
  }
  return(out)
}
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

  if(!exists("..shown", envir=., inherits = FALSE)) {
    ## all done here
  } else {
    ## need to write out the JS to show the handler
#    cat(.$writeHandlerJS(signal, lst), file=stdout())          # a single handler
    cat(.$writeHandlerJS(signal, lst))          # a single handler
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




