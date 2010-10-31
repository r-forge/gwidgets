##' @include gtable.R

##' Class for a data frame editor
setClass("gDf",
         contains="gGridComponent",
         prototype=prototype(new("gGridComponent"))
         )

##' Constructor for a data frame editor
##'
##' @exports
gdf <- function(
                items = NULL, name = deparse(substitute(items)), do.subset = FALSE,
                container = NULL, ... ,
                toolkit=guiToolkit()){
  widget <- .gdf (toolkit,
                  items=items, name=name, do.subset=do.subset, container=container ,...
                  )
  obj <- new( 'gDf',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gdf
setGeneric( '.gdf' ,
           function(toolkit,
                    items = NULL, name = deparse(substitute(items)),
                    do.subset = FALSE,      container = NULL, ... )
           standardGeneric( '.gdf' ))


## methods
## addHandlerChanged, addHandlerClicked, addHandlerDoubleClicked
## addHandlerColumnClicked, addHandlerColumnDoubleClicked
