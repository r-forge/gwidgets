##' @include guiComponent.R
##'
##'
##'
##' RangeSelector Class
setClass("guiComponentRangeSelector",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

## svalue, svalue<- [, [<-

##' spinbutton class
setClass("gSpinbutton",
         contains="guiComponentRangeSelector",
         prototype=prototype(new("guiComponentRangeSelector"))
         )

##' Spinbutton constructor
##'
##' @export
gspinbutton =function(
  from = 0, to = 10, by = 1, value = from, digits = 0,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){
  widget <- .gspinbutton (toolkit,
                          from=from, to=to, by=by, value=value, digits=digits,
                          handler=handler, action=action, container=container ,...
                          )
  obj <- new( 'gSpinbutton',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gspinbutton
setGeneric( '.gspinbutton' ,
           function(toolkit,
                    from = 0, to = 10, by = 1, value = from, digits = 0,
                    handler = NULL, action = NULL, container = NULL, ... ) standardGeneric( '.gspinbutton' ))
