##' @include manipulate.R
NULL

## Manipulate expressions that produce text output
TextManipulate <- setRefClass("TextManipulate",
                              fields=list(
                                widget="ANY"
                                ),
                              contains="Manipulate",
                              methods=list(
                                change_handler=function(...) {
                                  "Evaluate code with current values"
                                  values <- get_values()
                                  result <- withVisible(eval(.code, envir=values))
                                  if (result$visible) {
                                    svalue(widget) <<- capture.output(eval(result$value))
                                  }
                                },
                                execute=function() {
                                  "Make the GUI"
                                  gtoolkit()
                                  w <- gwindow("ManipulateR", visible=FALSE)
                                  g <- ggroup(cont=w, expand=TRUE, horizontal=FALSE)
                                  f <- gframe("Controls", cont=g)
                                  lyt <- glayout(cont=f)
                                  ## add controls

                                  sapply(.controls, function(i) {
                                    i$make_gui(cont=lyt, 
                                               handler=.self$change_handler)
                                  })
                                  widget <<- gtext("", cont=g, expand=TRUE, font.attr=c(family="monospace"))

                                  ## configure width option to match window size
                                  switch(gtoolkit(),
                                         "tcltk"={
                                           e <- getToolkitWidget(widget)
                                           tl <- tkwinfo("toplevel", e) 
                                           tkbind(tl, "<Configure>", function(W) {
                                             width <- as.numeric(tkwinfo("width", e))
                                             options(width=floor(width/8))
                                           })
                                         },
                                         "RGtk2"={
                                           require(RGtk2)
                                           e <- getToolkitWidget(widget)$getToplevel()
                                           e$addEvents(GdkEventType['configure'])
                                           gSignalConnect(e, "configure-event", function(...) {
                                             alloc <- getToolkitWidget(widget)$getAllocation()
                                             width <- alloc$allocation$width
                                             options(width=floor(width/8))
                                             FALSE
                                           })
                                         },
                                         "Qt"= {}
                                         )
                                  
                                  visible(w) <- TRUE
                                  change_handler()                    # initial
                                }
                                ))

##' Manipulate an expression that produces text output
##' 
##' @inheritParams manipulate
##' @export
##' @examples
##' \dontrun{
##' ## test it out
##' x <- rnorm(1:100, 10, 10); y <- rnorm(1:100, 5,5);
##' text_manipulate({
##' cut_it <- function(x, n) cut(x, seq(min(x), max(x), length.out=n))
##'   xtabs(~ cut_it(y, m) + cut_it(x, n))
##' }, m= slider(2, 20, 2, 1), n = slider(2, 20, 2, 1))
##' }
text_manipulate <- function(._expr,...) {
  obj <- TextManipulate$new(substitute(._expr),...)
  obj$execute()
  invisible(obj)
}
