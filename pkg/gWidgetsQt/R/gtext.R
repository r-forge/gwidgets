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

## begin
setClass("gTextQt",
         contains="gTextWidgetQt",
         prototype=prototype(new("gTextWidgetQt"))
         )

setMethod(".gtext",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text=NULL,
                   width=NULL, height=200,
                   font.attr = NULL, wrap = TRUE,
                   handler = NULL, action=NULL,
                   container=NULL, ...) {


            force(toolkit)

            ## options

            txt <- Qt$QTextEdit()
            
            obj <- new("gTextQt", block=txt, widget=txt, 
              toolkit=toolkit, ID=getNewID(), e = new.env())

            if(!is.null(text))
              svalue(obj) <- text

            if(!is.null(font.attr)) 
              font(obj) <- font.attr    # entire buffer, as no selection
            
            if(!as.logical(wrap))
              txt$setLineWrapMode(0)     # 0 = no wrap See Qt$LineWrapModel
            
            if(!is.null(width))
              ## width height in terms of characters??
              size(obj) <- c(width,height)
            
            if(!is.null(container))
              add(container, obj,...)

            ## add handler
            if (!is.null(handler)) {
              tag(obj, "handler.id") <- addhandlerkeystroke(obj, handler, action)
            }
            
            return(obj)
          })

## drop=TRUE to get only mouse selected text
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            w <- getWidget(obj)
            
            index <- getWithDefault(index, FALSE)
            drop <- getWithDefault(drop, FALSE)

            if(index) {
              ## rongui request, if INDEX = TRUE return selected text
              ## by index in the buffer
              tc <- w$textCursor()
              if(tc$hasSelection()) {
                out <- c(start=tc$selectionStart(),
                         end=tc$selectionEnd())
              } else {
                out <- NA
              }
              return(out)
            }

            ## otherwise we return text
            ## if drop=FALSE 
            ## if drop=TRUE, get selected text only
            if(drop) {
              val <- w$textCursor()$selectedText()
            } else {
              val <- w$toPlainText()
            }

            ## do we split into \n??
            
            return(val)
          })

## ##  svalue<-() replaces text from TextWidget
## setReplaceMethod(".svalue",
##                  signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
##                  function(obj, toolkit, index=NULL, ..., value) {
##                    w <- getWidget(obj)
##                    w$setText(paste(value, collapse="\n"))
                   
##                    return(obj)
##                  })


## clear all text in buffer
setMethod("dispose",signature(obj="gTextQt"),
          function(obj,...)  {
            .dispose(obj, obj@toolkit, ...)
          })
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
          function(obj, toolkit,  ...) {
            w <- getWidget(obj)
            w$clear()
          })


### Add method is a workhorse for this class. Value can be
## * a line of text
## * a vector of lines of text
## need to do where value of "point"
## add, as a method, needs to have a consistent signature. I'

## add text
setMethod(".insert",
          signature(toolkit="guiWidgetsToolkitQt",obj = "gTextQt"),
          function(obj, toolkit, value, where = c("end","beginning","at.cursor"),
                   font.attr = NULL,
                   do.newline = TRUE, ...) {
            ## just call add
            where <- match.arg(where)
            .add(obj, toolkit, value, where=where, font.attr=font.attr,
                 do.newline=do.newline, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt",value="character"),
          function(obj, toolkit, value,  ...) {
            w <- getWidget(obj)

            theArgs <- list(...)                      # look for font.attr, do.newline, where
            do.newline <- getWithDefault(theArgs$do.newline, TRUE)
            font.attr <-  getWithDefault(theArgs$font.attr, NULL)
            where <- getWithDefault(theArgs$where, "end")

            value <- paste(value, collapse="\n")
            if(do.newline)
              value = paste(value,"\n",sep="")

            ## add text, set selecdtion, adjust font, unselect
            tc <- w$textCursor()

            if(where == "beginning") {
              tc$movePosition(1L, 0L)        # 1=start, 0=move anchor
            } else if(where == "end") {
              tc$movePosition(11L, 0L)
            } else {
              tc$movePosition(tc$anchor(), 0)
            }
            
            if(!is.null(font.attr)) 
              tc$insertText(value, makeQTextCharFormat(font.attr))
            else
              tc$insertText(value)

            ## scroll viewport to cursor?
                        
          })

## add a widget
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt",value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj,toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt",value="gWidgetQt"),
          function(obj, toolkit, value,  ...) {
            cat("gtext: implement adding a widget to text area\n")
            return()
            })


## set the font for the selected area of the gtext object
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
                 function(obj, toolkit, ..., value) {
                   
                   w <- getWidget(obj)
                   tc <- w$textCursor()

                   ## make into a list
                   if(!is.list(value))
                     value <- lapply(value, function(i) i)
                   
                   f <- makeQFont(value)
                   
                   if(tc$hasSelection()) {
                     w$setCurrentFont(f)
                     if(!is.null(value$color))
                       w$setTextColor(Qt$QColor(value$color))
                   } else {
                     w$selectAll()
                     w$setCurrentFont(f)
                     if(!is.null(value$color))
                       w$setTextColor(value$color)
                     tc$clearSelection()
                     w$setTextCursor(tc)
                   }

                   return(obj)

                 })


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            .addhandlerkeystroke(obj,toolkit, handler,action, ...)
          })


## keystroke is called when widget display changes
## XXX This isn't quite right, but we inherit from gTextWidget
## setMethod(".addhandlerkeystroke",
##           signature(toolkit="guiWidgetsToolkitQt",obj="gTextQt"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             f <- function(txt, ...) {
##               h <- list(obj=obj, action=action)
##               n <- nchar(txt)
##               h$keystroke <- substr(txt, n, n)
##               handler(h)
##             }
##             ID <- qconnect(getWidget(obj), "textChanged", f)
##             invisible(ID)
##           })

