## XXX only for local

gfile <- function(text="Choose a file",
                  type = c("open"),
                  filter = NULL, 
                  handler = NULL, action = NULL, container = NULL, ...) {

  if(!gWidgetsWWWIsLocal())
    stop("Not for non-local user")

  
  widget <- EXTComponent$new(toplevel=container$toplevel,
                             ..text = text, ..type=type, ..filter=filter
                             )
  class(widget) <- c("gFile",class(widget))
  widget$setValue(value="")             # empty, set on fileselected
  widget$..width <- 400

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
                 
  widget$scripts <- function(.) {
    f <- system.file("javascript","ext.ux.form.fileuploadfield.js", package="gWidgetsWWW")
    out <- paste(readLines(f), collapse="\n")
    return(out)
  }
  
  ## methods
#  widget$getValueJSMethod <- "getValue"

#  widget$setValueJSMethod <- "setValue"
  ## widget$transportSignal <- c("fileselected")
  ## widget$transportValue <- function(.,...) {
  ##   out <- String() +
  ##     'var value = s;'
  ##   return(out)
  ## }
  
 ## widget$ExtConstructor <- "Ext.ux.form.FileUploadField"
 ## widget$ExtCfgOptions <- function(.) {
 ##   out <- list("fileupload"=TRUE,
 ##               width=.$..width,
 ##               defaults=list(
 ##                 anchor="100%"
 ##                 ),
 ##               items=list(
 ##                 xtype="fileuploadfield",
 ##                 emptytext=.$..text),
 ##               buttons = list(
 ##                 list(
 ##                      text="Save"
 ##                      ),
 ##                 list(
 ##                      text="reset",
 ##                      handler=String() + "function() {" + .$ID + ".getForm().reset();}"
 ##                      )
 ##                 )
 ##               )
 ##   out <- list(buttonOnly=TRUE)
 ##   return(out)
 ## }

  ## Try something else
  widget$ExtConstructor <- "Ext.FormPanel"
  widget$ExtCfgOptions <- function(.) {
    out <- list(fileUpload=TRUE,
#                height=30,
                frame=FALSE,
                autoHeight=TRUE,
                items=list(
                  width=.$..width,
                  xtype='fileuploadfield',
                  empytText='select a file',
                  buttonText='Browse...',
                  listeners=list(
                    "'fileselected'"=String("function(fb, v) {") + "\n" +
                    "_transportToR('" + .$ID + "'," + "\n" +
                    "Ext.util.JSON.encode({value:v})\n)\n}"
                    )
                  )
                )
    return(out)
  }

  
  if(!is.null(handler))
    widget$addHandlerClicked(handler=handler,action=action)
  

  

  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("fileselected",handler=handler,action=action)
  
  invisible(widget)
}

