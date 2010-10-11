##' @include guiComponents.R

##' single line text edit class
setClass("gAction",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' An action constructor
##'
##' @exports
gaction <- function(
                    label, tooltip=NULL, icon = NULL, key.accel = NULL,
                    handler = NULL, action = NULL, ...,
                    toolkit=guiToolkit()) {
  widget <- .gaction (toolkit,
                      label, tooltip, icon, key.accel, handler, action, ...
                      )
  obj <- new( 'gAction',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gaction
setGeneric( '.gaction' ,
           function(toolkit,
                    label, tooltip = NULL, icon = NULL, key.accel=NULL,
                    handler = NULL, action = NULL,
                    ... )
           standardGeneric( '.gaction' ))


