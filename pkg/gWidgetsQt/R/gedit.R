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


## class defined in aaaClasses for inheritance
## constructor

setClass("gTextWidgetQt",
         contains="gEventWidgetQt",
         prototype=prototype(new("gEventWidgetQt"))
         )

## setClass("gTextWidgetQt",
##          contains="gComponentQt",
##          prototype=prototype(new("gComponentQt"))
##          )

setClass("gEditQt",
         ## representation = representation("gComponentQt",
         ##   coercewith="NULLorFunction"),
         contains="gTextWidgetQt",
         prototype=prototype(new("gTextWidgetQt"))
         )

## qtConstructor
creategwClass("QLineEdit")


setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL,
                   initial.msg = "", 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {

           force(toolkit)
            

            ## check that coerce.with is a function
            if(is.null(coerce.with) || is.function(coerce.with)) {
              ## okay
            } else {
              if(is.character(coerce.with)) {
                coerce.with = get(coerce.with)
              }
            }

           
##           entry <- Qt$QLineEdit()
           entry <- gwQLineEdit()
           
           completer <- Qt$QCompleter()
           entry$setCompleter(completer)
           mod <- Qt$QStandardItemModel()
           completer$setModel(mod)

           obj <- new("gEditQt",block=entry, widget=entry,
                      toolkit=toolkit, e=new.env(), ID=getNewID())
#           ,coercewith=coerce.with)
           entry$setObject(obj)
           
           
           svalue(obj) <- text
           tag(obj, "coerce.with") <- coerce.with
           tag(obj, "default_fill") <- "x"
           tag(obj, "initial.msg") <- initial.msg

           if(nchar(text) == 0 && nchar(initial.msg) > 0) {
             ## set value
             entry$setStyleSheet(sprintf("* {color: %s}", "gray"))
             entry$setText(initial.msg)
             ## focus handler to remove id
             id <- entry$setEventHandler("focusInEvent", function(...) {
               if(entry$text == initial.msg)
                 entry$setText("")
               entry$setStyleSheet(sprintf("* {color: %s}", "black"))
             }, action="")
           }
           

           ## XXX no width argument. Use size<- instead
           if(!is.null(width)) 
             entry$setMinimumWidth(as.numeric(width) * QtCharacterWidth)

           
           ## Drag and drop
           ## addDropSource(obj)
           ## addDropTarget(obj)

           if(!is.null(container))
             add(container, obj,...)
           
            if (!is.null(handler)) 
              tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)
           
           
           invisible(obj)
            
            
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEditQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            val <- w$text
            if(val == tag(obj, "initial.msg")) val <- ""
            val <- do.coerce(val, tag(obj, "coerce.with"))
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gTextWidgetQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   w <- getWidget(obj)
                   w$setStyleSheet(sprintf("* {color: %s}", "black"))                   
                   w$setText(paste(as.character(value), collapse="\n"))
                   return(obj)
          })

setMethod("[",
          signature(x="gEditQt"),
          function(x, i, j, ..., drop=TRUE) {
            if(missing(i))
              .leftBracket(x,x@toolkit, ...)
            else
              .leftBracket(x,x@toolkit, i, ...)
          })

## left bracket implement completion
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gEditQt"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            w <- getWidget(x)
            completer <- w$completer()
            mod <- completer$model()
            n <- mod$rowCount()
            if(n == 0)
              return(character(0))
            else
              lapply(1:n, function(i) {
                item <- mod$item(i-1)
                item$text()
              })
          })

setReplaceMethod("[",
                 signature(x="gEditQt"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })


setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitQt",x="gEditQt"),
          function(x, toolkit, i, j, ..., value) {
            vals <- x[]
            if(missing(i))
              vals <- value
            else
              vals[i] <- value

            w <- getWidget(x)
            completer <- w$completer()
            mod <- completer$model()
            n <- length(vals)
            lapply(seq_len(n), function(i) {
              item <- Qt$QStandardItem(vals[i])
              mod$setItem(i - 1, item)
            })
            
            return(x)
          })


## setReplaceMethod(".size", 
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gEditQt"),
##                  function(obj, toolkit, ..., value) {
##                    return(obj)
##                  })



##################################################
## handlers

## changed is called after a commit
## XX might want to add blur here (lostFocus)
## editingFinished: return, enter or lose focus
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gEditQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="returnPressed", handler, action, ...)
            #.addHandler(obj, toolkit, signal="editingFinished", handler, action, ...)
          })

## keystroke is called when widget display changes
## XXX This isn't quite right
setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextWidgetQt"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ## Want this
            ## .addEventHandler(obj, "keyPressEvent", handler, action, "key")
            
            f <- function(txt, ...) {
              h <- list(obj=obj, action=action)
              n <- nchar(txt)
              h$key <- substr(txt, n, n)
              handler(h)
            }
            ID <- qconnect(getWidget(obj), "textChanged", f)
            invisible(ID)
          })

