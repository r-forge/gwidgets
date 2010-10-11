##' @include guiComponentWithItems.R

##' checkboxgroup class
setClass("gCheckboxGroup",
         contains="guiComponentWithItems",
         prototype=prototype(new("guiComponentWithItems"))
         )

##' Constructor for checkbox group. A linked group of checkboxes, but not exclusive.
##'
##' @export
gcheckboxgroup <- function(
                           items, checked = FALSE, horizontal = FALSE,
                           use.table=FALSE, handler = NULL,
                           action = NULL, container = NULL, ... ,
                           toolkit=guiToolkit()){
  widget <- .gcheckboxgroup (toolkit,
    items=items, checked=checked, horizontal=horizontal, use.table=use.table,
    handler=handler, action=action, container=container, ...
    )
  obj <- new( 'gCheckboxGroup',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
setGeneric( '.gcheckboxgroup' ,
           function(toolkit,
                    items, checked = FALSE, horizontal = FALSE,
                    handler = NULL, action = NULL,
                    container = NULL, ... ) standardGeneric( '.gcheckboxgroup' )) 
