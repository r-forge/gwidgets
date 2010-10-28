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
         'function(btn) {' +
           'if(btn == "ok") {' +
             ##           'alert(btn);' + ## makes a *big* difference -- why?
             'runHandlerJS('+handlerid + ',"","");' +
               '}'+
                 '}'
     } else if(type == "input") {
       ## Here we call the handler with the value from the widget passed in through
       ## h$context -- not h$input
       handlerFunction <- String() +
         'function(btn,text) {' +
           'if(btn == "ok") {' +
             'runHandlerJS('+handlerid + ',' +
               'Ext.util.JSON.encode({input:text})' +
                 ');' +
               '}'+
                 '}'
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
  if(!doanimEl) ## trouble with deep down handlers??????
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

## input -- can't figure out how to get handler the value of input
## likely needs to be set as a global variable
ginput <- function(message, text="", title="Input",
                   icon = c("info", "warning","error", "question"),
                   parent=NULL,
                   handler = NULL, action = NULL,...) {
  ## parent must be non-NULL
  out <- .gshowdialog(type="input",message=message,
                      title=title, icon=icon,parent=parent,
                      handler=handler, action=action,...)
}
  


gbasicdialog <- function(title = "Dialog", widget,
                         parent=NULL, handler = NULL, action=NULL) {
  stop("XXX not written")
}


##
## gfile -- upload a file (not find a file on filesytem)
## XXX this is different
gfile <- function() {
  stop("XXX write me to upload a file")
}



## quick alert message -- not modal or obtrusive (dropped from above in extjs)
galert <- function(message, title = "message", delay=3, parent=NULL) {
  ## parent not used here
  if(missing(message))
    message <- ""

  if(is.null(parent)) {
    parent <- gwindow("XXXX")
  }
  
  
  out <- String() +
    'Ext.example.msg(' + shQuoteEsc(title) + ',' +
      shQuoteEsc(message) + ',' + delay + ');'

  parent$addJSQueue(out)
#  cat(out)
#  invisible(out)
}
