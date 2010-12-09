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


##################################################
## Dialogs
## Dialogs are called from a handler. They output
## javascript code only.
## The respond to the handler

## parent can be a container or a widget
##' A basic dialog called by others
##'
##' 
##' @param type type of dialog
##' @param message message for dialog
##' @param text secondary text message. Ignored
##' @param title title is for title bar of dialog's window
##' @param icon icon to accompany dialog
##' @param parent Used for animation
##' @param handler Called when dialog is activated
##' @param action passed to handler
##' @param ... 
##' @param doanimEl gWidgetsWWW option. Logical. Do we animate?
.gshowdialog <- function(type=c("message","confirm","input"),
                        message, text, title=type,
                        icon = c("info","warning","error","question"),
                        parent,
                        handler = NULL, action = NULL,
                         ...,
                         doanimEl=TRUE) {

  ## make object to get ID, assign handlers to
  widget <- EXTWidget$new(toplevel=parent$toplevel)
  class(widget) <- c("gDialog",class(widget))
  ## since we don't "add" we pass in id and toplevel
  id <- widget$ID <- parent$toplevel$newID()
  widget$toplevel <- parent$toplevel

  widget$..defaultTextHeight <- 40      # if input
  
  ## fix icon
  if(missing(icon)) {
    icon = "QUESTION"
  } else {
    icon <- toupper(match.arg(icon))
  }

  ## Define handler callback
  handlerFunction = ""
   if(!is.null(handler)) {
     ## add handler and define string to call handler for constructor
     handlerid <- widget$addHandler(signal=NULL, handler=handler, action=action)
     if(type == "confirm") {
       handlerFunction <- String() +
         paste('function(btn) {',
               '  if(btn == "ok") {',
               sprintf('    runHandlerJS("%s","","");',handlerid),
               '  }',
               '}',
               sep="\n")
     } else if(type == "input") {
       ## Here we call the handler with the value from the widget passed in through
       ## h$context -- not h$input
       handlerFunction <- String() +
         paste('function(btn,text) {',
               '  if(btn == "ok") {',
               sprintf('runHandlerJS(%s,Ext.util.JSON.encode({input:text}));', handlerid), 
               '  }',
               '}',                     # no trailing ";"
               sep="\n")
     }
   }
     
  ## doesn't like \n below
  message <- gsub("\n","<p>",message)
  
  lst <- list(id = id,
              title = escapeHTML(title),
              msg = escapeHTML(message),
              buttons = String(ifelse(type == "message","Ext.Msg.CANCEL","Ext.Msg.OKCANCEL")),
              animEl = parent$ID,
              icon =  String("Ext.MessageBox.") + icon
              )
  if(!doanimEl) 
    lst[["animEl"]] <- NULL
  
  if(handlerFunction != "")
    lst[['fn']] = handlerFunction

  if(type == "input") {
    lst[["multiline"]] <- TRUE
    lst[["defaultTextHeight"]] <- widget$..defaultTextHeight
  }
  
  out <- sprintf("Ext.MessageBox.show(%s);\n",
                 widget$mapRtoObjectLiteral(lst))
  widget$addJSQueue(out)
}

##' A simple message dialog.
##' 
##' @param message main message.
##' @param title Title for dialog's window
##' @param icon icon to decorate dialog
##' @param parent parent container (the main window instance)
##' @param handler handler passed to dialog when confirmed
##' @param action action passed to handler
##' @param ... ignored
##' @export
gmessage <- function(message, title="message",
                     icon = c("info", "warning", "error", "question"),
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {
  ## parent must be non-NULL
  out <- .gshowdialog(type="message",message=message,
               title=title, icon=icon,parent=parent,
               handler=handler, action=action, ...)
  
}

##' Confirmation dialog
##' 
##' @param message message
##' @param title title for dialog's window
##' @param icon icon
##' @param parent parent container (main window instance)
##' @param handler handler passed to dialog if confirmed
##' @param action passed to any handler
##' @param ... ignored
##' @export
gconfirm <- function(message, title="Confirm",
                     icon = c("info", "warning", "error", "question"),
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {
  ## parent must be non-NULL
  .gshowdialog(type="confirm",message=message,
                      title=title, icon=icon,parent=parent,
                      handler=handler, action=action,...)
}

##' input dialog.
##'
##' Used for getting a text string to pass to a handler
##' @param message message
##' @param title title for dialog's window
##' @param icon icon
##' @param parent parent container (main window instance)
##' @param handler handler passed to dialog if confirmed
##' @param action passed to any handler
##' @param ... ignored
##' @export
ginput <- function(message, text="", title="Input",
                   icon = c("info", "warning","error", "question"),
                   parent=NULL,
                   handler = NULL, action = NULL,...) {
  ## parent must be non-NULL
  out <- .gshowdialog(type="input",message=message,
                      title=title, icon=icon,parent=parent,
                      handler=handler, action=action,...)
}
  

##' means to turn a widget into a dialog
##'
##' Not written
##' @param title title
##' @param widget widget
##' @param parent parent
##' @param handler handler
##' @param action action
gbasicdialog <- function(title = "Dialog", widget,
                         parent=NULL, handler = NULL, action=NULL) {
  stop("XXX not written")
}


## gfile in gfile.R

##' quick alert message -- not modal or obtrusive (dropped from above in extjs)
##' 
##' @param message message to display
##' @param title title of message
##' @param delay delay in seconds
##' @param parent parent window, typically gwindow instance. Necessary
galert <- function(message, title = "message", delay=3, parent=NULL) {
  ## parent not used here
  if(missing(message))
    message <- ""

  message <- paste(message, collapse="<br />")
  
  if(is.null(parent)) {
    stop("Needs parent")
  }
  
  out <- sprintf("Ext.example.msg(%s, %s, %s);",
                 shQuoteEsc(title),
                 shQuoteEsc(message),
                 delay)

  parent$addJSQueue(out)
}
