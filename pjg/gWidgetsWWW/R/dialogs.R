##################################################
## Dialogs
## Dialogs are called from a handler. They output
## javascript code only.
## The respond to the handler

### XXX for some reason, dialogs don't call other dialogs?
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
  
  out <- String() +
    'Ext.MessageBox.show(' + widget$mapRtoObjectLiteral(lst) + ');' + '\n';

  return(out)

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
  cat(out)
  invisible(out)
}

gconfirm <- function(message, title="Confirm",
                     icon = c("info", "warning", "error", "question"),
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {
  ## parent must be non-NULL
  out <- .gshowdialog(type="confirm",message=message,
                      title=title, icon=icon,parent=parent,
                      handler=handler, action=action,...)
  cat(out)
  invisible(out)
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
  cat(out, file=stdout())
  invisible(out)
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
  out <- String() +
    'Ext.example.msg(' + shQuoteEsc(title) + ',' +
      shQuoteEsc(message) + ',' + delay + ');'

  cat(out)
  invisible(out)
}
