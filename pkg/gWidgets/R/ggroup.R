##' @include guiContainer.R

##' Base class for box containers
setClass("gGroup",
         contains="guiContainer",
         prototype=prototype(new("guiContainer"))
         )

##' Constructor for horizontal or vertical box container
##'
##' @export
ggroup <- function(
                   horizontal = TRUE, spacing = 5, use.scrollwindow = FALSE, container = NULL, ... ,
                   toolkit=guiToolkit()){
  widget <- .ggroup (toolkit,
                     horizontal=horizontal, spacing=spacing,
                     use.scrollwindow = use.scrollwindow, 
                     container=container,...
                     )
  obj <- new( 'gGroup',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias ggroup
setGeneric( '.ggroup' ,
           function(toolkit,
                    horizontal = TRUE, spacing = 5,  use.scrollwindow = FALSE, container = NULL,  ... )
           standardGeneric( '.ggroup' ))



################## Methods ###############################

################## addSpace ################################
##' addSpace generic for box containers
setGeneric("addSpace",function(obj,value, ...) standardGeneric("addSpace"))

## addspace method
setMethod("addSpace",signature(obj="gGroup"),
          function(obj, value, ...) {
            toolkit = obj@toolkit
            .addSpace(obj@widget,toolkit,value,...)
          })

##' dispatch with toolkit
##' @alias addSpace
setGeneric(".addSpace",function(obj,toolkit,value,...) standardGeneric(".addSpace"))


##################################################
##' addSpring generic
setGeneric("addSpring",function(obj,...) standardGeneric("addSpring"))

##' addSpring method for box containers
setMethod("addSpring",signature(obj="gGroup"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .addSpring(obj@widget, toolkit,...)
          })

##' dispatch with toolkit
##' @alias addSpring
setGeneric(".addSpring",function(obj, toolkit,...) standardGeneric(".addSpring"))


