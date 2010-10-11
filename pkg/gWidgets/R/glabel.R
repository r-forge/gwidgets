##' @include guiComponents.R

##' Label class
setClass("gLabel",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' constructor for label widget
glabel = function(
  text= "", markup = FALSE, editable = FALSE, handler = NULL, 
    action = NULL, container = NULL, 
  ..., toolkit=guiToolkit()) {
  widget = .glabel(toolkit,
    text= text, markup = markup, editable = editable, handler = handler, 
    action = action, container = container, 
    ...)
  obj = new("gLabel",widget=widget,toolkit=toolkit)
  return(obj)
}

##' glabel generic for toolkit
setGeneric(".glabel",function(toolkit,
                              text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                              action = NULL, container = NULL, 
                              ...) standardGeneric(".glabel"))
