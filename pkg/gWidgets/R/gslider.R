##' @include gspinbutton.R

##' slider class
setClass("gSlider",
         contains="guiComponentRangeSelector",
         prototype=prototype(new("guiComponentRangeSelector"))
         )

##' slider widget constructor
##'
##' @param from If a number of length one then a starting point, in
##' which case to, by are passed to \code{seq}. Otherwise a sequence
##' of values for which sort(unique(from)) will order
##' @param to ending point when from is starting point
##' @param by step size if not specified by \code{from}
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


##' svalue referst o value of slider
##'
##' @param index if non \code{NULL} and \code{TRUE} and sequence specified at once then index
