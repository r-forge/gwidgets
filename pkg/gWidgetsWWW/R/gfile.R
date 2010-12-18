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


## XXX only for local

##' File selection function
##'
##' This allows a local user to select a file. It does not do file
##' upload (yet!).  The \code{svalue} method only returns the
##' filename, not the path to the file. The behaviour under some
##' browser, such as Chrome, actually puts in a fakepath.
##' @param text Instructional text. Ignored.
##' @param type only "open" implemented
##' @param filter ignored
##' @param handler called when file is selected
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to add method of parent container
gfile <- function(text="Choose a file",
                  type = c("open"),
                  filter = NULL, 
                  handler = NULL, action = NULL, container = NULL, ...) {

  if(!gWidgetsWWWIsLocal())
    stop("Not for non-local user")

  
  widget <- EXTComponentNoItems$new(toplevel=container$toplevel,
                             ..text = text, ..type=type, ..filter=filter
                             )
  class(widget) <- c("gFile", class(widget))
  widget$setValue(value="")             # empty, set on fileselected
  widget$..width <- getFromDots(..., var="width", default=300) # width is funny

  widget$emptyText <- text;#'select a file'
  widget$buttonText <- 'Browse...'
  
  ## CSS
  widget$css <- function(.) {
    out <- paste(
                 ## from http://www.extjs.com/deploy/dev/examples/form/file-upload.html
                 ##                 "/*",
                 ##                 "* FileUploadField component styles",
                 ##                 "*/",
                 ".x-form-file-wrap {",
                 "position: relative;",
                 "height: 22px;",
                 "}",
                 ".x-form-file-wrap .x-form-file {",
                 "position: absolute;",
                 "right: 0;",
                 "-moz-opacity: 0;",
                 "filter:alpha(opacity: 0);",
                 "opacity: 0;",
                 "z-index: 2;",
                 "height: 22px;",
               "}",
                 ".x-form-file-wrap .x-form-file-btn {",
                 "position: absolute;",
                 "right: 0;",
                 "z-index: 1;",
               "}",
                 ".x-form-file-wrap .x-form-file-text {",
                 "position: absolute;",
                 "left: 0;",
                 "z-index: 3;",
                 "color: #777;",
               "}",
                 sep=" ")
    return(out)
  }
                 
  ## methods
  widget$getValueJSMethod <- "getValue"
  widget$setValueJSMethod <- "setValue"
#  widget$ExtConstructor <- "Ext.ux.form.FileUploadField"
  
  widget$transportSignal <- c("fileselected")
  widget$transportValue <- function(.,...) {
    out <- 'var value = s;'
    return(out)
  }

widget$ExtConstructor <- "Ext.FormPanel"
widget$ExtCfgOptions <- function(.) {
  out <- list(fileUpload=TRUE,
                                      #                height=30,
              frame=FALSE,
              autoHeight=TRUE,
              items=list(
                xtype='fileuploadfield',
                width=.$..width,
                empytText=.$emptyText,
                buttonText=.$buttonText
                )
              )
  return(out)
}

widget$asCharacterPanelName <- function(.) .$asCharacter() + "Panel"
widget$..writeConstructor <- function(.) {
  out <- String() +
    sprintf("%s = %s.getComponent(0);", .$asCharacter(), .$asCharacterPanelName())
}


  

  ## add after CSS, scripts defined
  container$add(widget,...)
  
  widget$addHandlerChanged <- function(., handler, action=NULL) 
    .$addHandler("fileselected",handler=handler,action=action)
  
  if(!is.null(handler))
    widget$addHandlerChanged(handler, action)
  
  invisible(widget)
}

