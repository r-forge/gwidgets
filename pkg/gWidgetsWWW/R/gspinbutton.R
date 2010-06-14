## Works as of Ext-3.0
## why are there no arrows???

## others
gspinbutton <- function(from = 0, to = 100, by = 1, value = from,
                    handler = NULL, action = NULL, container = NULL, ...) {
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..from = from, ..to = to, ..by=by
                             )
  class(widget) <- c("gSpinbutton",class(widget))
  widget$setValue(value=value)
  widget$..coerce.with="as.numeric"
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
                 ".x-form-spinner-proxy{",
                 "/*background-color:#ff00cc;*/",
                 "}",
                 ".x-form-field-wrap .x-form-spinner-trigger {",
                   "background:transparent url('../images/spinner.gif') no-repeat 0 0;",
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
                 "background:transparent url('../images/spinner-split.gif') no-repeat 0 0;",
                 "position:absolute;",
                 "cursor: n-resize;",
               "}",
                 ".x-trigger-wrap-focus .x-form-spinner-splitter{",
                 "background-position:-14px 0;",
               "}",
                 sep="")
    return(out)
  }
  
  widget$scripts <- function(.) {
    out <- String()

    ## These should be in a javascript directory on the web server,
    ## but this is easier (slower too as it doesn't cache)
    f <- system.file("javascript","ext.ux.spinner.js", package="gWidgetsWWW")
    out <- out + "\n" + paste(readLines(f), collapse="\n")
    
    f <- system.file("javascript","ext.ux.spinnerformfield.js", package="gWidgetsWWW")
    out <- out + "\n" + paste(readLines(f), collapse="\n")
    
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


  if(!is.null(handler))
    widget$addHandler("change",handler=handler,action=action)
  
  invisible(widget)
}

