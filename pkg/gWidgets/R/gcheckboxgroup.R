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



##' svalue method for checkboxgroup
##'
##' Main property of checkbox group are the states. 
##' @param obj object
##' @param index If \code{TRUE} returns indices of those checked. Otherwise a logical vector.
##' @param drop ignored
##' @return logical vector or vector of indices
##' @exports
setMethod("svalue", signature(obj="gCheckboxGroup"),
          function(obj, index=NULL, drop=NULL, ... ) {
            .svalue(obj@widget, obj@toolkit, ...,index=index, drop=drop)            
          })




##' set state for checkbox
##'
##' @param obj
##' @param index if \code{TRUE} the indices are expected. If \code{FALSE} logical vector.
##' @param ... ignored
##' @param value numeric or logical depending on \code{index}
##' @return void
##' @exports
setReplaceMethod("svalue", signature(obj="gCheckboxGroup"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj@widget, obj@toolkit, index=index, ...) <- value
            return(obj)
          })
