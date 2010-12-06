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

## Works as of Ext-3.0
## why are there no arrows???

## others

##' Basic spinbutton
##'
##' For some reason the images don't work!
##' @param from from value
##' @param to to
##' @param by by. From to by are same as seq() usage
##' @param value initial value
##' @param handler handler for when change is made
##' @param action passed to the handler, if given
##' @param container parent container
##' @param ... passed to add method of parent
##' @export
##' @examples /Tests/test-gspinbutton.R
##' @TODO get icons working. Not sure why they dont (positioning is the likely culprit)
gspinbutton <- function(from = 0, to = 100, by = 1, value = from,
                    handler = NULL, action = NULL, container = NULL, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by
                             )
  class(widget) <- c("gSpinbutton",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"

  ## no index
  widget$assignValue <- function(., value) {
    .$..data <- as.numeric(value[[1]])
  }

  ## CSS
  ## XXX Get this css to work to get trigger icon for spin
  ##   widget$css <- function(.) {
##     out <- String()
##     f <- system.file("css","ext.ux.spinner.css", package="gWidgetsWWW")
##     out <- out + "\n" + paste(readLines(f), collapse="\n")
##     out
##   }
  widget$css <- function(.) {
    ## can't have comments etc., as this goes into one line.
    out <- paste(
#                 ".x-form-spinner-proxy{",
#                 "/*background-color:#ff00cc;*/",
#                 "}",
                 ".x-form-field-wrap .x-form-spinner-trigger {",
                   sprintf("background:transparent url('%s/spinner.gif') no-repeat 0 0;",gWidgetsWWWimageUrl),
                 "}",
                 ".x-form-field-wrap .x-form-spinner-overup{",
                 "background-position:-17px 0;",
               "}",
                 ".x-form-field-wrap .x-form-spinner-clickup{",
                 "background-position:-34px 0;",
               "}",
                 ".x-form-field-wrap .x-form-spinner-overdown{",
                 "background-position:-51px 0;",
               "}",
                 ".x-form-field-wrap .x-form-spinner-clickdown{",
                 "background-position:-68px 0;",
               "}",
                 "",
                 "",
                 ".x-trigger-wrap-focus .x-form-spinner-trigger{",
                 "background-position:-85px 0;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-overup{",
                 "background-position:-102px 0;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-clickup{",
                 "background-position:-119px 0;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-overdown{",
                 "background-position:-136px 0;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-clickdown{",
                 "background-position:-153px 0;",
               "}",
                 ".x-trigger-wrap-focus .x-form-trigger{",
                 "border-bottom: 1px solid #7eadd9;",
               "}",
                 "",
                 ".x-form-field-wrap .x-form-spinner-splitter {",
                 "line-height:1px;",
                 "font-size:1px;",
                 sprintf("background:transparent url('%s/spinner-split.gif') no-repeat 0 0;",gWidgetsWWWimageUrl),
                 "position:absolute;",
                 "cursor: n-resize;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-splitter{",
                 "background-position:-14px 0;",
               "}",
                 sep="")
    return(out)
  }
  ## methods
  widget$getValueJSMethod <- "getValue"

  widget$setValueJSMethod <- "setValue"
  widget$transportSignal <- c("spin")
  widget$ExtConstructor <- "Ext.ux.form.SpinnerField"
  widget$ExtCfgOptions <- function(.) {
    out <- list("value"= svalue(.),
                "minValue" =  .$..from,
                "maxValue" = .$..to,
                "allowDecimals"=TRUE,
                "decimalPrecision"=1,
                "accelerate"=TRUE,
                "incrementValue" = .$..by,
                "enableKeyEvents"=TRUE,
                "triggerClass"='x-form-spinner-trigger',
                "splitterClass"='x-form-spinner-splitter'
                )

    return(out)
  }


  ## add after CSS, scripts defined
  container$add(widget,...)

  widget$addHandlerChanged <- function(., handler, action=NULL) 
    .$addHandler(signal="spin",handler, action)
  

  if(!is.null(handler))
    addHandlerChanged(widget, handler, action)
  
  invisible(widget)
}

