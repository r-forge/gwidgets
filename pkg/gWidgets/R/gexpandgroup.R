##' @include gframe.R

##' Class for a box container with disclosure trigger
setClass("gExpandGroup",
         contains="gFrame",
         prototype=prototype(new("gFrame"))
         )

##' Constructor of box container widget with disclosure trigger and label
##'
##' @export
gexpandgroup <- function(
                         text = "", markup = FALSE, horizontal=TRUE,
                         handler = NULL, action = NULL,
                         container = NULL, ... ,
                         toolkit=guiToolkit()){
  widget <- .gexpandgroup (toolkit,
                           text=text, markup=markup, horizontal=horizontal,
                           handler=handler, action=action, container=container ,...
                           )
  obj <- new( 'gExpandGroup',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gexpandgroup
setGeneric( '.gexpandgroup' ,
           function(toolkit,
                    text = "", markup = FALSE,horizontal=TRUE,
                    handler = NULL, action = NULL,
                    container = NULL, ... )
standardGeneric( '.gexpandgroup' ))
