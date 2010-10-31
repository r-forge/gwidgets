##' @include guiComponentWithItems.R

##' Combobox class
setClass("gCombobox",
         contains="guiComponentWithItems",
         prototype=prototype(new("guiComponentWithItems"))
         )

##' constructor for combobox
##'
##' @export
gcombobox =function(
  items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL,      action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gdroplist (toolkit,
    items=items, selected=selected, editable=editable, coerce.with=coerce.with, handler=handler, action=action, container=container, ...
    )
  obj = new( 'gCombobox',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##'
##' @alias gcombobox
setGeneric( '.gdroplist' , function(toolkit,
                                    items, selected = 1, editable = FALSE, coerce.with = NULL, handler = NULL,      action = NULL, container = NULL, ... ) standardGeneric( '.gdroplist' ))

##' Alias for gcombobox. Deprecated
##' 
##' @alias gcombobox
gdroplist <- gcombobox 



##' svalue method for combobox
##'
##' Main property of checkbox group are the states. 
##' @param obj object
##' @param index If \code{TRUE} returns index of selected. Otherwise a character vector.
##' @param drop ignored
##' @return character or index. Some toolkits return \code{-1} when nothing is selected.
##' @exports
setMethod("svalue", signature(obj="gCombobox"),
          function(obj, index=NULL, drop=NULL, ... ) {
            .svalue(obj@widget, obj@toolkit, ...,index=index, drop=drop)            
          })




##' set state for combobox
##'
##' @param obj
##' @param index if \code{TRUE} then an index is expected. If \code{FALSE}, then name should match available unless \code{editable=TRUE} is specified
##' @param ... ignored
##' @param value numeric or character depending on \code{index}
##' @return void
##' @exports
setReplaceMethod("svalue", signature(obj="gCombobox"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj@widget, obj@toolkit, index=index, ...) <- value
            return(obj)
          })
