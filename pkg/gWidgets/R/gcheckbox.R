##' @include guiComponents.R

##' Checkbox class
setClass("gCheckbox",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' constructor for checkbox widget
gcheckbox =function(
  text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcheckbox (toolkit,
    text=text, checked=checked,
    use.togglebutton=use.togglebutton,
    handler=handler, action=action, container=container, ...
    )
  obj = new( 'gCheckbox',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' Generic for toolkit dispatch
setGeneric( '.gcheckbox' , function(toolkit,
                                    text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL,
                                    container = NULL, ... ) standardGeneric( '.gcheckbox' ))



##' svalue method for checkbox
##'
##' Main property of checkbox is the state \code{TRUE} or \code{FALSE}
##' @param obj object
##' @param index ignored
##' @param drop ignored
##' @return logical.
##' @exports
setMethod("svalue", signature(obj="gCheckbox"),
          function(obj, index=NULL, drop=NULL, ... ) {
            .svalue(obj@widget, obj@toolkit, ...,index=index, drop=drop)            
          })




##' set state for checkbox
##'
##' @param obj
##' @param index ignored
##' @param ... ignored
##' @param value logical indicating if button is checked or not
##' @return void
##' @exports
setReplaceMethod("svalue", signature(obj="gCheckbox"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj@widget, obj@toolkit, index=index, ...) <- value
            return(obj)
          })

