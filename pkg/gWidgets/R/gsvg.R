##' @include guiComponents.R

##' Class for widget to display SVG data
setClass("gSvg",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' construtor for widget to display svg data
##'
##' @exports
gsvg <- function(
                 filename="", width=480, height=480,
                 handler=NULL, action=NULL,
                 container = NULL, ... ,
                 toolkit=guiToolkit()){
  widget <- .gsvg (toolkit,
                   filename=filename, width=width, height=height,
                   handler=handler, action=action, container=container ,...
                   )
  obj <- new( 'gSvg',widget=widget,toolkit=toolkit) 
  return(obj)
}

##' generic for toolkit dispatch
##' @alias gsvg
setGeneric( '.gsvg' ,
           function(toolkit,
                    filename = "", width=480,  height=480,
                    handler=NULL, action=NULL,
                    container = NULL, ... )
           standardGeneric( '.gsvg' ))


##' gsvg constructor for ANY class (default to label)
setMethod(".gsvg",
          signature(toolkit="ANY"),
          function(toolkit,
                   filename = "", width=480, height=480,
                   handler=NULL, action=NULL,
                   container = NULL,
                   ...) {
            cat(gettext("gsvg widget not imlemented"))
            return(glabel(container=container, ...))
          })
