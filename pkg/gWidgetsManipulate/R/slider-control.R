##' @include controls.R
NULL


## Slider class
Slider <- setRefClass("Slider",
                      contains="ManipulateControls",
                      methods=list(
                        validate_inputs = function(min, max, initial, step, ticks, label) {
                          ## validate inputs
                          if (!is.numeric(initial) || !is.numeric(min) || !is.numeric(max))
                            stop("min, max, amd initial must all be numeric values")
                          else if (initial < min)
                            stop(paste("slider initial value", initial, "is less than the specified minimum"))
                          else if (initial > max)
                            stop(paste("slider initial value", initial, "is greater than the specified maximum"))
                          else if (min > max)
                            stop(paste("slider maximum is greater than minimum"))
                          else if ( !is.null(step) ) {
                            if ( !is.numeric(step) )
                              stop("step is not a numeric value")
                            if ( step > (max - min) )
                              stop("step is greater than range")
                          } else if ( !is.logical(ticks) )
                            stop("ticks is not a logical value")
                        },
                        initialize=function(min, max, initial=min, label=NULL, 
                          step=-1, ticks=TRUE) {
                          validate_inputs(min, max, initial, step, ticks, label)
                          ## create slider and return it
                          slider <- list(type = 0,
                                         min = min,
                                         max = max,
                                         step = step,
                                         ticks = ticks)
                          callSuper(l=slider, label=label, initial=initial)
                        },
                        make_gui=function(cont, handler, ...) {
                          n <- dim(cont)[1]
                          cont[n+1,1] <- label
                          if(l$step < 0) l$step <<- 1
##                          g <- ggroup(cont=cont, horizontal=TRUE, ...)
                          cont[n+1, 2] <- (g <- ggroup(cont=cont, horizontal=TRUE, ...))
                          widget <<- gslider(from=l$min, to=l$max, by=l$step, cont=g)
                          ## toolkit adjustments
                          switch(gtoolkit(),
                                 "RGtk2" = {
                                   size(widget) <<- c(200, -1)
                                 },
                                 "tcltk" = {
                                   ## add label to slider
                                   slider_label <- glabel(svalue(widget), cont=g)
                                   addHandlerChanged(widget, function(h,...) svalue(slider_label) <- svalue(h$obj))
                                 }
                                 )
                          callSuper(cont, handler)
                        }))

                      
##' Slider interface
##' 
##' @param min minimum value
##' @param max maximum value
##' @param initial initial value. Must be in min <= max
##' @param step step size. Use -1 for default
##' @param ticks logical. are ticks drawn?
##' @param label optional label for control
##' @return return Slider instance
##' @export
slider <- function(min, max, initial=min, 
                   step=-1, ticks=TRUE,
                   label="") {

  Slider$new(min, max, initial=initial, label=label, 
             step=step, ticks=ticks)
}

