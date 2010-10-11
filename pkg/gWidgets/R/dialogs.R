##' @include guiComponents



##' Alert dialog to display transient messages
galert = function(
  message,
  title = "message",
  delay = 3,
  parent = NULL, 
  ..., toolkit=guiToolkit()) {
  .galert(toolkit,message, title, 
            ...)
}
##' generic for toolkit dispatch
##' @alias galert
setGeneric(".galert",
           function(toolkit,
                    message, title="message", delay=3, parent=NULL, ...)
           standardGeneric(".galert"))


##' Constructor for modal message dialog
##' 
##' @export
gmessage = function(
  message,
  title = "message",
  icon = c("info", "warning", "error", "question"),
  parent=NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .gmessage(toolkit,message, title, icon, parent, handler, action,
            ...)
}

##' generic for toolkit dispatch
##' @alias gmessage
setGeneric(".gmessage",
           function(toolkit,
                    message=message, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".gmessage"))

############## ginput ####################################

##' Constructor for modal dialog to collect a line of text
##'
##' @export
ginput <- function(
  message, text="",
  title = "Input",
  icon = c("info", "warning", "error", "question"),
  parent = NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .ginput(toolkit,
          message, text=text, title=title, icon, parent, handler, action, 
          ...)
}

##' generic for toolkit dispatch
##' @alias ginput
setGeneric(".ginput",
           function(toolkit,
                    message=message, text=text, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".ginput"))

################# gconfirm #################################
##' Constructor for modal dialog to get confirmation
##'
##' @export
gconfirm = function(
  message,
  title = "Confirm",
  icon = c("info", "warning", "error", "question"),
  parent=NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  .gconfirm(toolkit,message=message, icon=icon, parent=parent, handler=handler, action=action,
            ...)
}

##' generic for toolkit dispatch
##' @alias gconfirm
setGeneric(".gconfirm",
           function(toolkit,
                    message=message, title=title, icon=icon,
                    parent = parent,
                    handler=handler, action=action, ...)
           standardGeneric(".gconfirm"))

################# gbasicdialog #################################

## define subclass for basic dialog
setClass("guiDialog",
         contains="guiContainer",
         prototype=prototype(new("guiContainer"))
         )




##' Constructor for modal dialog that can contain an arbitrary widget
##'
##' @export
gbasicdialog <- function(
  title = "Dialog", widget,
  parent = NULL,
  handler = NULL, action = NULL,
  ..., toolkit=guiToolkit()) {
  if(missing(widget)) {
    obj <- .gbasicdialognoparent(toolkit, title, parent, handler, action,...)
    obj <- new( 'guiDialog',widget=obj,toolkit=toolkit) 
  } else {
    obj <- .gbasicdialog(toolkit,
                  title=title, widget=widget,parent=parent,
                  handler=handler, action=action,
                         ...)
  }
  return(obj)
}

##' generic for toolkit dispatch
##' @alias gbasicdialog
setGeneric(".gbasicdialog",
           function(toolkit,
                    title = "Dialog", widget, parent,
                    handler = NULL, action = NULL,
                    ...)
           standardGeneric(".gbasicdialog"))

##' generic for toolkit dispatch when there is no parent
##' @alias gbasicdialog
setGeneric(".gbasicdialognoparent",
           function(toolkit,
                    title = "Dialog",  parent,
                    handler = NULL, action = NULL,
                    ...)
           standardGeneric(".gbasicdialognoparent"))



