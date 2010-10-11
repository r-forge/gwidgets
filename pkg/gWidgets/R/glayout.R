##' @include guiContainer.R

##' Class for a gridded-layout container
setClass("gLayout",
         contains="guiContainer",
         prototype=prototype(new("guiContainer"))
         )

##' Constructor for grid layout container
##'
##' @export
glayout <- function(
                    homogeneous = FALSE, spacing = 10, container = NULL,      ... ,
                    toolkit=guiToolkit()){
  widget <- .glayout (toolkit,
                      homogeneous=homogeneous, spacing=spacing, container=container ,...
                      )
  obj <- new( 'gLayout',widget=widget,toolkit=toolkit) 
  return(obj)
}

##' generic for toolkit dispatch
##' @alias glayout
setGeneric( '.glayout' ,
           function(toolkit,
                    homogeneous = FALSE, spacing = 10, container = NULL,
                    ... )
           standardGeneric( '.glayout' ))

