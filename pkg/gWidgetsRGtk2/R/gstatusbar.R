## gtkStatusBar. Use value to push message, value to pop
setClass("gStatusbarRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )
## constructor
setMethod(".gstatusbar",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", container=NULL, ...) {

            force(toolkit)
            
            statusbar <- gtkStatusbarNew()
            statusbar$setHasResizeGrip(TRUE)
            statusbar$push(statusbar$getContextId("message"), text)

            obj <- as.gWidgetsRGtk2(statusbar)
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj,...)
            }
  
            invisible(obj)
          })

as.gWidgetsRGtk2.GtkStatusbar <- function(widget,...) {
  obj <- new("gStatusbarRGtk",block=widget, widget=widget,
             toolkit=guiToolkit("RGtk2"))
  return(obj)
}


### methods

## This pops from stack
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gStatusbarRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            obj@widget$Pop(obj@widget$getContextId("message"))
          })

## This pushes to stack
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gStatusbarRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj@widget$Push(obj@widget$getContextId("message"), value)
                   return(obj)
                 })

