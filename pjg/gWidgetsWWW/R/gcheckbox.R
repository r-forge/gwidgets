## gcheckbox
## uses a button with different shading (YAHOO style)
## methods
## svalue works
## svalue<- works
## names 
## names<-  NO METHOD setBoxLabel
gcheckbox = function(text, checked = FALSE,
  handler = NULL, action = NULL,  container = NULL,...) {
  
  widget <- EXTComponentInPanel$new(toplevel=container$toplevel)
  class(widget) <- c("gCheckbox",class(widget))
  widget$setValue(value=checked)
  widget$setValues(value = text)

  ## give a default size, as otherwise panel will spread across screen
  widget$..width <- 200                 # use size()<- to set otherwise
  
  ## define methods

  ## this returns via cat, javascript to set the buttons value
  widget$coerce.with = function(.,x) {
    if(is.character(x)) x <- toupper(x)
    return(as.logical(x))
  }
  widget$getValueJSMethod = "getValue"
  widget$setValueJSMethod = "setValue"
  widget$transportSignal <- "check"   

  ## rather than use   widget$ExtConstructor <- "Ext.form.Checkbox"
  ## we use EXTComponentInPanel and set the xtype here
  widget$ExtCfgOptions <- function(.) {
    list(xtype = "checkbox",
         renderTo = NULL,               # override value
         id = as.character(String(.$ID) + "item"),
         "checked" = svalue(.),
         "boxLabel" = .$getValues()[1]
         )
  }


  ## add after CSS, scripts defined
  container$add(widget,...)


  if(!is.null(handler))
    widget$addHandler("check",handler=handler,action=action)

  invisible(widget)
}

### A check box group

