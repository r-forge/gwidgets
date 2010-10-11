##' @include guiComponents.R

##' constructor to display tabular data for selection
setClass("gTable",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' A constructor for displaying tabular data for selection
##'
##' @exports
gtable <- function(
                   items, multiple = FALSE, chosencol = 1, icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL, filter.FUN = NULL,
                   handler = NULL, action = NULL, container = NULL, ... ,
                   toolkit=guiToolkit()){
  widget <- .gtable (toolkit,
                     items=items, multiple=multiple, chosencol=chosencol,
                     icon.FUN=icon.FUN,
                     filter.column=filter.column, filter.labels=filter.labels, filter.FUN=filter.FUN,
                     handler=handler, action=action, container=container ,...
                     )
  obj <- new( 'gTable',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gtable
setGeneric( '.gtable' ,
           function(toolkit,
                    items, multiple = FALSE, chosencol = 1,
                    icon.FUN = NULL,
                    filter.column = NULL, filter.labels = NULL, filter.FUN = NULL,
                    handler = NULL, action = NULL, container = NULL, ... )
           standardGeneric( '.gtable' ))

