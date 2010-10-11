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
