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


## this adds text to toplevel's ..statusBarText
## and puts widget into ..statusBar so that ..shown can be set.
## if toplevel is a
## statusbar is different from gstatusbar -- does not pop to last message
gstatusbar <- function(text = "", container=NULL, ...) {

  widget <- EXTComponentNoItems$new(toplevel=container$toplevel)
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


  ## ## need to load in ux code, as of 3.0 status bar is not ext native
  ## widget$scripts <- function(.) {
  ##   f <- system.file("javascript","ext.ux.statusbar.js", package="gWidgetsWWW")
  ##   out <- paste(readLines(f), collapse="\n")
    
  ##   return(out)
  ## }

  
  ## helper to get status bar by its ID
  widget$getSBJS <- function(.) {
    out <- String() +
      'var widget = Ext.getCmp("' +
        .$sbContainer$ID + 'statusBar' + '");'
    return(out)
  }

  widget$setValue <- function(., index=NULL, ..., value) {
    .$..data <- value
    if(exists("..shown",envir=., inherits=FALSE)) 
      .$addJSQueue(.$setValueJS(index=index, ...))
  }
  widget$setValueJS <- function(.,...) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)

    out <- paste(sprintf("var widget = Ext.getCmp('%sstatusBar');", .$sbContainer$ID),
                 sprintf("widget.setText(%s);", ourQuote(svalue(.))),
                 sep="")

    ## 'text:' + shQuote(svalue(.)) + ',' +
    ##        'clear: true, iconCls: "x-status-valid" });' + '\n'
    
    return(out)
  }

  ## XXX Extra API
  ## a statusbar had *lots* of other things we could do here through Ext:
  ## menus, toolbars, busy signals, a clear after a certain time period
  ## we add these here as extra methods not in API
  widget$showBusy <- function(., text="Busy...") {
    out <- 
      paste(sprintf("var widget = Ext.getCmp('%sstatusBar');", .$sbContainer$ID),
            sprintf('widget.setBusyText("%s");', text),
            sep="")
    
    .$addJSQueue(out)
  }
  widget$clearBusy <- function(.) {
    out <- 
      paste(sprintf("var widget = Ext.getCmp('%sstatusBar');", .$sbContainer$ID),
            'widget.setBusyText("");',
            sep="")
    
    .$addJSQueue(out)
  }
  widget$clearStatus <- function(.) {
    out <- paste(sprintf("var widget = Ext.getCmp('%sstatusBar');", .$sbContainer$ID),
          'widget.clearStatus();',
          sep="")
#    out <- .$getSBJS() +
#      'widget.clearStatus();';
#      sprintf('widget.showStatus({text: "%s", iconCls:"x-status-valid"});', .$getValue())
    .$addJSQueue(out)
  }

  invisible(widget)
}
