##' @include guiComponents.R

##' a calendar class for date selection
setClass("gCalendar",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' A constructor for a date selection widget
##'
##' @exports
gcalendar <- function(
                      text = "", format = "%Y-%m-%d", 
                      handler = NULL, action=NULL, container = NULL,...,
                      toolkit=guiToolkit()){
  widget <- .gcalendar (toolkit,
                        text=text, format=format, handler=handler,action=action,
                        container=container , ...
                        )
  obj <- new( 'gCalendar',widget=widget,toolkit=toolkit) 
 return(obj)
}


##' generic for toolkit dispatch
##' @alias gcalendar
setGeneric( '.gcalendar' ,
           function(toolkit,
                    text = "", format = "%Y-%m-%d", 
                    handler=NULL, action=NULL, container = NULL,
                    ... )
           standardGeneric( '.gcalendar' ))
