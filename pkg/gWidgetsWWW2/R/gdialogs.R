##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' @include ext-widget.R
NA

##' A simple message dialog.
##' 
##' @param message main message.
##' @param title Title for dialog's window
##' @param icon icon to decorate dialog
##' @param parent parent container (the main window instance)
##' @param handler Ignored. handler passed to dialog when confirmed
##' @param action Ignored. action passed to handler
##' @param ... ignored
##' @return return value ignored
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##' gmessage("Hello there", parent=w)
##' })
gmessage <- function(message, title="message",
                     icon = NULL,
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {

  dlg <- GDialog$new(parent$toplevel)
  icon <- sprintf("Ext.MessageBox.%s", toupper(match.arg(icon,c("info", "warning", "error", "question"))))

  cmd <- sprintf("Ext.Msg.show({title:%s, msg:%s, buttons:Ext.Msg.CANCEL, icon:%s, animEl:'%s'});",
                 ourQuote(title),
                 ourQuote(message),
                 icon,
                 parent$id)
  dlg$add_js_queue(cmd)
}


##' Confirmation dialog
##'
##' Calls handler when Yes button is clicked. Unlike other gWidgets
##' implementations, this one does block the R process before
##' returning a logical indicating the selection. One must use a
##' handler to have interactivity.
##' @param message message
##' @param title title for dialog's window
##' @param icon icon
##' @param parent parent container (main window instance)
##' @param handler handler passed to dialog if confirmed
##' @param action passed to any handler
##' @param ... ignored
##' @return return value ignored, use handler for response
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##' gconfirm("Do you like R", parent=w, handler=function(h,...) {
##' galert("Glad you do", parent=w)
##' })
##' })
gconfirm <- function(message, title="Confirm",
                     icon = NULL,
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {

  dlg <- GDialog$new(parent$toplevel)
  cbid <- dlg$toplevel$add_R_handler(dlg, handler, action, ...)

  
  icon <- sprintf("Ext.MessageBox.%s", toupper(match.arg(icon,c('info', 'warning', 'error', 'question'))))

  fn <- sprintf("function(buttonID, text, o) {if(buttonID == 'yes') {callRhandler('%s')}}", cbid)

  
  cmd <- sprintf("Ext.Msg.show({title:%s, msg:%s, buttons:Ext.Msg.YESNO, icon:%s, animEl:'%s', fn:%s});",
                 ourQuote(title),
                 ourQuote(message),
                 icon,
                 parent$id,
                 fn
                 )
  dlg$add_js_queue(cmd)

}



             
##' input dialog.
##'
##' Used for getting a text string to pass to a handler. Unlike other
##' gWidgets implementations, this call does not block the R process,
##' so any response to the user must be done through the handler.
##' @param message message
##' @param text initial text for the widget
##' @param title title for dialog's window
##' @param icon icon ignored
##' @param parent parent container (main window instance)
##' @param handler Called if yes is selected, the component
##' \code{input} of the first argument holds the user-supplied string.
##' @param action passed to any handler
##' @param ... ignored
##' @return return value ignored, use handler for response
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##'   ginput("What is your name?", parent=w, handler=function(h,...) {
##'     galert(paste("Hello", h$input), parent=w)
##'   })
##' })
ginput <- function(message, text="", title="Input",
                   icon = NULL,
                   parent=NULL,
                   handler = NULL, action = NULL,...) {

  dlg <- GDialog$new(parent$toplevel)
  cbid <- dlg$toplevel$add_R_handler(dlg, handler, action, ...)

  
  fn <- paste("function(buttonID, text) {",
              "if(buttonID == 'ok') {",
              sprintf("callRhandler('%s', {input:text})",
                      cbid),
              "}}",
              sep="")

  
  cmd <- sprintf("Ext.Msg.prompt(%s, %s, %s, this, true, %s);",
                 ourQuote(title),
                 ourQuote(message),
                 fn,
                 ourQuote(text)
                 )
  dlg$add_js_queue(cmd)
}

##' Base class for dialogs
GDialog <- setRefClass("GDialog",
                       contains="ExtWidget")




##' quick alert message -- not modal or obtrusive (dropped from above in extjs)
##' 
##' @param message message to display
##' @param title title of message
##' @param delay delay in seconds
##' @param parent parent window, typically gwindow instance. Necessary
##' @return not used
##' @export
##' @examples
##' w <- gwindow()
##' b <- gbutton("click me", cont=w, handler=function(h,...) {
##' galert("That hurt", parent=w)
##' })
##' 
galert <- function(message, title = "message", delay=3, parent) {

  dlg <- GDialog$new(parent$toplevel)

  ## parent not used here
  if(missing(message))
    message <- ""
  
  message <- paste(message, collapse="<br />")
  
  cmd <- sprintf("Ext.example.msg(%s, %s, %s);",
                 ourQuote(title),
                 ourQuote(message),
                 delay)

  dlg$add_js_queue(cmd)
  
  
}

### TODO: implement gbasicdialog