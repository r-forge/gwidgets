## this adds text to toplevel's ..statusBarText
## and puts widget into ..statusBar so that ..shown can be set.
## if toplevel is a
## statusbar is different from gstatusbar -- does not pop to last message
gstatusbar <- function(text = "", container=NULL, ...) {

  widget <- EXTComponent$new(toplevel=container$toplevel)
  class(widget) <- c("gStatusbar",class(widget))

  widget$setValue(value=text)

  ## where to put. Here we make a distinction between
  ## subwindows and windows (which are toplevel component
  if(inherits(container,"gSubwindow")) {
    widget$sbContainer <- container
  } else {
    widget$sbContainer <- widget$toplevel
  }

  widget$sbContainer$..statusBarText <- text
  widget$sbContainer$..statusBar <- widget
  

  ## for statusbar, we get ID not from adding, but directly
  widget$ID <- container$newID()


  ## need to load in ux code, as of 3.0 status bar is not ext native
  widget$scripts <- function(.) {
    f <- system.file("javascript","ext.ux.statusbar.js", package="gWidgetsWWW")
    out <- paste(readLines(f), collapse="\n")
    
    return(out)
  }

  
  ## helper to get status bar by its ID
  widget$getSBJS <- function(.) {
    out <- String() +
      'var widget = Ext.getCmp("' +
        .$sbContainer$ID + 'statusBar' + '");'
    return(out)
  }
    
  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
    
    out <- String() +
      .$getSBJS() +
        'widget.setStatus({' +
          'text:' + shQuote(svalue(.)) + ',' +
            'clear: true, iconCls: "x-status-valid" });' + '\n'
    
    return(out)
  }

  ## XXX Extra API
  ## a statusbar had *lots* of other things we could do here through Ext:
  ## menus, toolbars, busy signals, a clear after a certain time period
  ## we add these here as extra methods not in API
  widget$showBusy <- function(.) {
    out <- .$getSBJS() +
      'widget.showBusy();';
    cat(out, file=stdout())
  }
  widget$clearStatus <- function(.) {
    out <- .$getSBJS() +
      'widget.clearStatus();';
    cat(out, file=stdout())
  }

  invisible(widget)
}
