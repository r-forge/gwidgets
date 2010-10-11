##' @include guiComponents.R

##' single line text edit class
setClass("gEdit",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' Single line text edit constructor
##'
##' @exports
gedit <- function(
                  text = "", width = 25, coerce.with = NULL,
                  handler = NULL, action = NULL, container = NULL, ... ,
                  toolkit=guiToolkit()){
  widget <- .gedit(toolkit,
                   text=text, width=width, coerce.with=coerce.with,
                   handler=handler, action=action, container=container ,...
                   )
  obj <- new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gedit
setGeneric( '.gedit' ,
           function(toolkit,
                    text = "", width = 25, coerce.with = NULL,
                    handler = NULL,action = NULL, container = NULL, ... )
           standardGeneric( '.gedit' ))

