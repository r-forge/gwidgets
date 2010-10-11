##' @include gspinbutton.R

##' slider class
setClass("gSlider",
         contains="guiComponentRangeSelector",
         prototype=prototype(new("guiComponentRangeSelector"))
         )

##' slider widget constructor
##'
##' @export
gslider <- function(
                    from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ... ,
                    toolkit=guiToolkit()){
  widget <- .gslider (toolkit,
                      from=from, to=to, by=by, value=value, horizontal=horizontal,
                      handler=handler, action=action, container=container ,...
                      )
  obj <- new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}
 
##' generic for toolkit dispatch
##' @alias gslider
setGeneric( '.gslider' ,
           function(toolkit,
                    from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ... ) standardGeneric( '.gslider' ))
