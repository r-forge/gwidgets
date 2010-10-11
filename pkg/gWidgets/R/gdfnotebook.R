##' @include guiComponents.R

##' class to hold a notebook of data frame editors
setClass("gDfNotebook",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' 
##'
##' @exports
gdfnotebook <- function(
                        items = NULL, container = NULL, ... ,
                        toolkit=guiToolkit()){
  widget <- .gdfnotebook (toolkit,
                          items=items, container=container ,...
                          )
  obj <- new( 'gDfNotebook',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gdfnotebook
setGeneric( '.gdfnotebook' ,
           function(toolkit,
                    items = NULL, container = NULL, ... )
           standardGeneric( '.gdfnotebook' ))
