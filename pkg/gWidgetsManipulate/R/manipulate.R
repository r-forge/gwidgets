## manipulate for gWidgets
##
## Original license for manipulate package
##
## Copyright (C) 2009-11 by RStudio, Inc.
##
## This program is licensed to you under the terms of version 3 of the
## GNU Affero General Public License. This program is distributed WITHOUT
## ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
## MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
## AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
##
##
## THe main point of AGPL:
##   The GNU Affero General Public License is designed specifically to
## ensure that, in such cases, the modified source code becomes available
## to the community.  It requires the operator of a network server to
## provide the source code of the modified version running there to the
## users of that server.  Therefore, public use of a modified version, on
## a publicly accessible server, gives the public access to the source
## code of the modified version.


##' Resolve variable arguments
##'
##' @param args args passed in
##' @return variables
##' @export
##' @author from RStudio manipulate package
resolveVariableArguments <- function(args) {
  # if the first argument is an unnamed list then just use this list
  if ( (length(args) == 1L) &&
       is.list(args[[1L]])  &&
       (is.null(names(args)) || (names(args)[[1L]] == "")) )  {
    return (args[[1L]])
  } else {
    return (args)
  }
}

##' Map a vector to a control
##'
##' @param x vector defining control
##' @return an instance of ManipulateControls or an error
mapVectorToControl <- function(x) UseMethod("mapVectorToControl")

##' Default control is to stop
##' 
##' @method mapVectorToControl default
##' @S3method mapVectorToControl default
mapVectorToControl.default <- function(x) stop(sprintf("No control defined for object of class %s", class(x)[1]))

##' numeric to  slider
##' @method mapVectorToControl numeric
##' @S3method mapVectorToControl numeric
mapVectorToControl.numeric <- function(x) {
  ## check if a sequence
  if(length(x) > 4) {
    y <- diff(diff(x))
    if(!all.equal(y, rep.int(0, length(y))))
      message("Expecting an arithmatic sequence, forcing it")
    n <- length(x); x <- sort(x)
    return(slider(x[1], x[n], x[1], diff(x)[1]))
  } else {
    ## min, max, step=1, inital=min
    if(length(x) == 2) {
      x[3] <- 1; x[4] <- x[1]
    } else if(length(x) == 3) {
      x[4] <- x[1]
    }
    return(slider(x[1], x[2], x[4], x[3]))
  }
}

##' logical maps to checkbox
##'
##' @method mapVectorToControl logical
##' @S3method mapVectorToControl logical
mapVectorToControl.logical <- function(x) {
  checkbox(initial=x[1], label="")
}

##' character to picker
##'
##' @method mapVectorToControl character
##' @S3method mapVectorToControl character
mapVectorToControl.character <- function(x) {
  do.call(picker, lapply(x, identity))
}


## Reference class for manipulate object
Manipulate <- setRefClass("Manipulate",
                          fields=list(
                            .code="ANY",
                            .controls="list",
                            dev="ANY"
                            ),
                          methods=list(
                            validate_controls=function() {
                              "Validate that controls are specified properly"
                              ##' From RStudio code
                              ## validate that all controls have unique names
                              controlNames <- names(.controls)
                              duplicatedIndex <- anyDuplicated(controlNames)
                              if (duplicatedIndex > 0)
                                stop(paste("duplicated control name:", controlNames[[duplicatedIndex]]))
                              
                              ## iterate over the names and controls, adding the default values to the env
                              for (name in names(.controls)) {
                                ## check the name
                                if (name == "")
                                  stop("all controls passed to manipulate must be named")
                                ## confirm that this is in fact a control
                                if(!is(.controls[[name]], "ManipulateControls")) {
                                  ## not a control, try to make a control
                                  .controls[[name]] <<- mapVectorToControl(.controls[[name]])
                                }
                                ## check
                                if(!is(.controls[[name]], "ManipulateControls")) {
                                  stop("Couldn't map control")
                                }
                                ## default label is control name
                                if(nchar(.controls[[name]]$label) == 0) 
                                  .controls[[name]]$label <<- name
                              }
                            },
                            get_values=function() {
                              "Get widget values as list"
                              sapply(.controls, function(i) i$get_value(), 
                                     simplify=FALSE)
                            },
                            change_handler=function(...) {
                              "Evaluate code with current values"
                              if(is.null(.code)) return()
                              values <- get_values()
                              
                              switch(gtoolkit(),
                                     "tcltk"={
                                       .my.tkdev(dev$hscale, dev$vscale)
                                       result <- withVisible(eval(.code, envir=values))
                                       if (result$visible) {
                                         eval(print(result$value))
                                       }
                                       .Tcl(paste("image create Rplot", dev$image))
                                     },
                                     {             
                                       result <- withVisible(eval(.code, envir=values))
                                       if (result$visible) {
                                         eval(print(result$value))
                                       }
                                     }
                                     )
                            },
                            execute=function() {
                              "Make the GUI"
                              w <- gwindow(gettext("ManipulateR"), visible=FALSE)
                              pg <- gpanedgroup(cont=w)
                              g <- ggroup(cont=pg, expand=TRUE)
                              if(gtoolkit()=="tcltk") {
                                require(tkrplot)
                                dev <<- tkrplot(getToolkitWidget(g), function() {})
                                add(g, dev, expand=TRUE)
                              } else {
                                ggraphics(cont=g, expand=TRUE, fill=TRUE)
                              }
                              f <- gframe(gettext("Controls"), cont=pg)
                              lyt <- glayout(cont=f)
                              ## add controls
                              sapply(.controls, function(i) {
                                i$make_gui(cont=lyt, 
                                           handler=.self$change_handler)
                              })
                              visible(w) <- TRUE
                              svalue(pg) <- 0.75
                              change_handler()                    # initial
                            },
                            initialize=function(code=NULL, ...) {
                              controls <- gWidgetsManipulate:::resolveVariableArguments(list(...))
                              initFields(.code=code,
                                         .controls=controls)
                              validate_controls()
                              callSuper()
                            }))
                          
##' Manipulate command ala RStudio
##'
##' @param ._expr expression to produce output.
##' @param ... used to specify controls. See \code{picker},
##' \code{checkbox}, \code{slider}.
##'
##' These controls may also be specified through a object, from which the control is guessed.
##' A logical maps to \code{checkbox}.
##' A character maps to \code{picker}.
##' A numeric to \code{slider}. This mapping can be specified as an arithmetic sequence of
##' points (length 5 or greater), or as a numeric vector of length 2 to 4 with defaults
##' like: \code{c(min, max, step=1, initial=min)}
##' @return makes output, returns Manipulate object
##' @export
##' @examples
##' \dontrun{
##' ## from RStudio::manipulate
##' manipulate(## expression
##'            plot(cars, xlim = c(x.min, x.max), type = type, 
##'                 axes = axes, ann = label),
##'            ## controls
##'            x.min = slider(0,15),
##'            x.max = slider(15,30, initial = 25),
##'            type = picker("p", "l", "b", "c", "o", "h", "s"),
##'            axes = checkbox(TRUE, label="Draw Axes"),
##'            label = checkbox(FALSE, label="Draw Labels")
##'            )
##' ## using shortcuts, ala Mathematica's manipulate
##' manipulate(## expression
##'            plot(cars, xlim = c(x.min, x.max), type = type, 
##'                 axes = axes, ann = label),
##'            ## controls
##'            x.min = 0:15,
##'            x.max = c(15,30, 1, 25), ## min, max, step=min, initial=min
##'            type = c("p", "l", "b", "c", "o", "h", "s"),
##'            axes = TRUE,
##'            label = FALSE
##'            )
##' }
manipulate <- function(._expr,...) {
  obj <- Manipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}

