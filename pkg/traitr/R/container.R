##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

#' @include controller.R
roxygen()

## Layouts for specifying how items get put

## Container objects are for layouts.
#' Base Trait for Container objects.
#'
#' Basic container is a glayout object for tabular layout
Container <- BaseTrait$proto(class=c("Container", BaseTrait$class),
                              .doc_container=paste(
                                desc("property to store base gWidgets container")
                                ),
                              container=NULL,  # stores base container
                              children=list(),     # stores children
                              type="glayout",
                              attr=list(),
                              .doc_context=paste(
                                desc("Context for container to look up values, such as whether to draw a label",
                                     "Usually inherited from calling object")
                                ),
                              context=NULL,
                              no_cols=1, # for glayout containers
                              .doc_is_realized=paste(
                                desc("Method to check if container has been realized (drawn as GUI)")
                              ),
                              is_realized=function(.) !is.null(.$container) && isExtant(.$container),

                              make_container=function(., container, attr=.$attr) {
                                ## check if container is traitr object
                                if(is.proto(container) && exists("traitr", container)) {
                                  if(container$is("Container"))
                                    container <- container$container # should be gWidgets object
                                  else
                                    stop(gettext("The container argument, if a proto object, needs to be a Container"))
                                }
                                
                                .$validate_ui()
                                
                                if(.$type=="") {
#                                  .$container <- cont
                                  return(container) # for aContext
                                }

                                attr$container <- container

                                if(.$type == "glayout")
                                  attr$spacing=0
                                

                                if(inherits(container,"gLayout") ||
                                   (inherits(container,"guiContainer") && grepl("^gLayout", class(container@widget)))
                                   ) {
                                  row_no <- get_with_default(tag(container, "row_no"), 1)
                                  col_no <- get_with_default(tag(container, "col_no"), 1)
                                  no_cols <- get_with_default(tag(container, "no_cols"), 1)
                                  row_no <- tag(container, "row_no")

                                  theSize <- attr$size; attr$size <- NULL
                                  container[row_no, 2*(col_no-1) + 1:2] <- (new_cont <- do.call(.$type, attr))
                                  visible(new_cont) <- TRUE
                                  
                                  if(!is.null(theSize))
                                    size(new_cont) <- theSize
                                  tag(container, "row_no") <- row_no + (col_no == no_cols)
                                  tag(container, "col_no") <- (col_no %% no_cols) + 1
                                } else {
                                  ## if a notebook grab the label property
                                  if(inherits(container, "gNotebook") ||
                                     (inherits(container, "guiContainer") && grepl("^gNotebook", class(container@widget)))
                                     ) {
                                    attr$label <- get_with_default(.$label, "")
                                  }
                                  theSize <- attr$size; attr$size <- NULL
                                  new_cont <- do.call(.$type, attr)
                                  visible(new_cont) <- TRUE
                                  if(!is.null(theSize))
                                    size(new_cont) <- theSize
                                  ## catch gWidgetsWwW and other
                                  if(inherits(new_cont,"gLayout") ||
                                     (inherits(new_cont,"guiContainer") &&grepl("^gLayout", class(new_cont@widget)))
                                     ) {
                                    tag(new_cont, "row_no") <- 1
                                    tag(new_cont, "col_no") <- 1
                                    tag(new_cont, "no_cols") <- .$no_cols
                                  }
                                }
                                .$container <- new_cont
                                new_cont
                              },
                             .doc_init_ui=paste(
                               desc("If non <code>NULL</code> called on intial drawing of container")
                               ),
                             init_ui=function(.) {},
                              ## context an ItemGroup instance to look up strings in.
                              .doc_make_ui=paste(
                                desc("Method to make user interface. Makes container,",
                                     "then makes UI for each child"),
                                param("cont","gWidgets container to place user interface into"),
                                param("attr", "Attribute list for gWidgets constructor"),
                                param("context","Context to find labels, etc from")
                                ),
                             make_ui = function(., container, attr=.$attr, context, ...) {
                                if(missing(context)) {
                                  if(!is.null(.$context))
                                    context <- .$context
                                  else
                                    context <- .
                                }
                                cont <- .$make_container(container)
                                
                                sapply(.$children, function(i) {

                                  if(is.null(i))
                                    return()

                                  ## first, if a character, convert to the object
                                  if(is.character(i)) {
                                    i <- context$get_item_by_name(i)
                                    if(is.null(i)) {
                                      cat(sprintf("Can't find item named %s", i))
                                      return()
                                    }
                                  }
                                  if(!is.proto(i)) {
                                    stop("Should be Item, ItemGroup or Container")
                                  }
                                  ## Now dispatch one of several ways.
                                  ## either an Item, ItemGroup or Container
                                  
                                  if(is.proto(i) && i$is("Item")) {
                                    i$make_ui(cont)
                                    i$init_ui()
                                  } else if(is.proto(i) && i$is("ItemGroup")) {
                                    i$make_ui(cont) ## XXX, gui_layout=i$make_default_gui_layout())
                                  } else if(is.proto(i) && i$is("Container")) {
                                    ## if non-trivial context in container, pass in
                                    if(i$has_slot("context") && !is.null(i$context)) {
                                      i$make_ui(cont)
                                    } else {
                                      i$context <- context
                                      i$make_ui(cont)
                                    }
                                  }
                                })
                                ## intialize ui if present
                                .$do_call("init_ui")
                                ## update if present
                                .$update_ui()
                              },
                              ## return TRUE to make container sensitive to events, FALSE to disable
                              .doc_enabled_when=paste(
                                desc("Method defining when a container should be enabled (sensitive to user",
                                     "input. Returns logical")
                                ),
                              enabled_when = function(.) {
                                ## . is from context
                                TRUE
                              },
                              ## method to call to make container enabled/disabled
                              enabled = function(., value) {
                                enabled(.$container) <- value
                              },
                              ## checked to see if container should be visible
                              .doc_visible_when=paste(
                                desc("Method to call to check if container should be visible.",
                                     "Returns a logical")
                                ),
                              visible_when = function(.) { TRUE },
                              ## method to call to make container visible/invisible
                              visible = function(., value) {
                                visible(.$container) <- value
                              },
                              .doc_validate_ui=paste(
                                desc("Method called by make_ui",
                                     "Gives warning if not valid")
                                ),
                              validate_ui=function(.) {},
                              ## override to make this update
                              .doc_update_ui=paste(
                                desc("Method to update user interface")
                                ),
                              update_ui = function(.) {
                                if(.$is_realized()) {
                                  if(!is.null(.$children) && length(.$children) > 0)
                                    sapply(.$children, function(i) {
                                      if(is.character(i)) {
                                        i <- .$context$get_item_by_name(i)
                                        if(is.null(i)) {
                                          cat(sprintf("Can't find item named %s", i))
                                          return()
                                        }
                                      }
                                        
                                      if(is.proto(i))
                                        i$do_call("update_ui", list())
                                    })

                                
                                  ## set enabled
                                  ## call passing in the context (an itemgroup, say), not
                                  ## the container
                                  
                                  ## wrap in try -- sometimes issue with update_ui call before
                                  ## model values are initialized
                                  try({
                                    .$enabled(.$get_slot("enabled_when")(.$context))
                                    .$visible(.$get_slot("visible_when")(.$context))
                                  }, silent=TRUE)
                                }
                              },
                             .doc_instance=paste(
                               desc("A copy of container that can be reused.")
                               ),
                             instance=function(.) {
                               ## clear out container, context
                               obj <- .$proto()
                               obj$container <- NULL
                               obj$context <- NULL
                               ## For each child, call instance if not character
                               for(i in seq_along(length(obj$children))) {
                                 j <- obj$children[[i]]
                                 if(is.proto(j))
                                   obj$children[[i]] <- j$do_call("instance")
                               }
                               return(obj)
                             }
                             )

## Various container constructors

## this is technical to give some children a context
#' A container to give a different context than the default for a set of items
#'
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export
aContext <- function(..., 
                     context, attr=list(),
                     enabled_when, visible_when) {
  obj <- Container$proto(children=list(...),
                         attr=attr,
                         type="",              # empty so no parent made
                         context=context)
  obj$class <- c("Context", obj$class)
  obj$is_realized <- function(.) TRUE
  obj$visible <- function(., value) {
    ## apply to children
    invisible(
              sapply(.$children, function(i) {
                if(is.null(i))
                  return()
                
                if(is.character(i)) {
                  i <- context$get_item_by_name(i)
                  if(is.null(i)) {
                    cat(sprintf("Can't find item named %s", i))
                    return()
                  }                  
                }
                if(!is.proto(i)) {
                  stop("Should be Item, ItemGroup or Container")
                }
                i$visible(value)
              })
              )
  }
  obj$enabled <- function(., value) {
    ## apply to children
    invisible(
              sapply(.$children, function(i) {
                if(is.null(i))
                  return()
                if(is.character(i)) {
                  i <- context$get_item_by_name(i)
                  if(is.null(i)) {
                    cat(sprintf("Can't find item named %s", i))
                    return()
                  }                  
                }
                if(!is.proto(i)) {
                  stop("Should be Item, ItemGroup or Container")
                }
                i$enabled(value)
              })
              )
  }       
  
  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}

## Basic container uses a "1" column table layout
#' A container to give a different context than the default for a set of items
#'
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

#' @examples
#' \dontrun{
#' i <- anItemGroup(x=numericItem(1), y=stringItem("a"))
#' lay <- aContainer("x","y")
#' makeGUI(i, gui_layout=lay)
#' }

aContainer <- function(..., context=NULL, attr=list(), enabled_when, visible_when) {
  obj <- Container$proto(children=list(...),
                         attr=list(),
                         context=context)
  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}

## Same as aContainer, only one can specify the number of columns
#' A container for tabular layout
#'
#' @param no_cols Number of columns. Fills in row by row.
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aTableLayout <- function(..., no_cols=1,# no_cols is really 2 * no_cols, we don't count labels here
                         context=NULL, attr=list(), enabled_when, visible_when) { 
  obj <- Container$proto(class=c("TableContainer", Container$class),
                         children=list(...),
                         no_cols=no_cols,
                         context=context,
                         attr=attr)
  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj

}

## a box container
#' A box container. Packs in items left to right or top to bottom
#'
#' @param horizontal If \code{TRUE} left to right, if \code{FALSE} top to bottom
#' @param spacing Space in pixels between items
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aGroup <- function(..., horizontal=TRUE, spacing=10,
                   context=NULL, attr=list(), enabled_when, visible_when) {
  obj <- Container$proto(children=list(...),
                  type="ggroup",
                  context=context, attr=attr
                  )
  ## issue with lazy eval if done in constructor  
  obj$attr <- merge(list(horizontal=horizontal, spacing=spacing), attr, overwrite=FALSE) 

  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}

#' Box container with label and visual separator to indicate grouping
#'
#' @param label label for frame
#' @param horizontal If \code{TRUE} left to right, if \code{FALSE} top to bottom
#' @param spacing Space in pixels between items
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aFrame <- function(..., label="frame label", horizontal=FALSE, spacing=10,
                   context=NULL, attr=list(), enabled_when, visible_when) {
  obj <- Container$proto(class=c("Frame",Container$class),
                  children=list(...),
                  type="gframe",
                  context=context)
  obj$attr <- merge(list(text=label, horizontal=horizontal, spacing=spacing), attr)

  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}


## XXX Need to work out how update_ui is done, put in model
## An expanding group with a trigger to show/hide its children
#' Expanding group. Has trigger to show/hide its children
#'
#' @param label label for trigger
#' @param horizontal If \code{TRUE} left to right, if \code{FALSE} top to bottom
#' @param expanded Initial state of children. Set to \code{TRUE} to show
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

anExpandGroup <- function(..., label="", horizontal=FALSE, expanded=TRUE,
                          context=NULL, attr=list(), enabled_when, visible_when) {
  ## set expanded property to toggle
  obj <- Container$proto(children=list(...),
                  type="gexpandgroup",
                  context=context,
                  expanded=expanded,
                  update_ui=function(.) {
                    visible(.$container) <- .$expanded
                  })
  obj$attr <- merge(list(text=label, horizontal=horizontal), attr)

  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}                    

#' A two panel paned group container.
#'
#' @param horizontal If \code{TRUE} left to right, if \code{FALSE} top to bottom
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aPanedGroup <- function(..., horizontal=TRUE,
                        context=NULL, attr=list(), enabled_when, visible_when) {
  obj <- Container$proto(children=list(...),
                  type="gpanedgroup",
                  context=context,
                  validate_ui=function(.) {
                    if(length(.$children) !=2)
                      warning("aPanedGroup should have two children only")
                  }
                  )
  obj$attr <- merge(list(horizontal=horizontal), attr)
  
  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when
  obj
}

#' A notebook container.
#'
#' Pages of notebook are aNotebookPage container, which in turn can hold other items
#' @param close_buttons Logical indicating if close buttons should be added (RGtk2 only)
#' @param initial_page Which page to open on
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aNotebook <- function(..., close_buttons=FALSE, initial_page=1,
                      context=NULL, attr=list(),  enabled_when, visible_when) {
  obj <- Container$proto(children=list(...),
                         type="gnotebook",
                         initial_page=initial_page,
                         context=context,
                         validate_ui = function(.) {
                           out <- sapply(.$children, function(i) {
                             if(is.character(i) ||
                                !i$is("NotebookPage"))
                               FALSE
                             else
                               TRUE
                           })
                           if(any(!out))
                             warning(gettext("Notebook children should be aNotebookPage instances"))
                         },
                         init_ui = function(.) svalue(.$container) <- initial_page
                         )
  obj$attr <- merge(list(closebuttons=close_buttons), attr)

  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when

  return(obj)
}

#' A page in a notebook
#'
#' @param label Tab label
#' @param context ItemGroup or item to get context from
#' @param attr gWidget values passed to constructor
#' @param enabled_when Method to determine when items in container should be enabled
#' @param visible_when Method to determine when items in container should be visible
#' @param ... children items specified by character strings
#' @return Returns a \code{proto} object. Call \code{obj$show_help()} to view its methods and properties.
#' @export

aNotebookPage <- function(..., label,
                          context=NULL, attr=list(),
                          enabled_when, visible_when) {
  obj <- aContainer(..., context=context, attr=attr)
  obj$add_class("NotebookPage")
  
  obj$label <- label
  if(!missing(enabled_when)) obj$enabled_when <- enabled_when
  if(!missing(visible_when)) obj$visible_when <- visible_when

  return(obj)
}