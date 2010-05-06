setClass("gQtObjectQt",
         contains="gComponentQt",
         prototype=prototype(new("gComponentQt"))
         )

##' qgtobject turns an instancde of RQtObject into a gwidget
##' so that it can be inserted into a GUI and inherit the methods
##' of a gwidget
gqtobject <- function(qtobj, container=NULL, ...) {
  if(!is(qtobj, "RQtObject")) {
    warning("Object is not a RQtObject instance")
    return(NULL)
  }

  obj <- new("gQtObjectQt",
             block=qtobj, widget=qtobj,
             toolkit=guiToolkit("Qt"),  e=new.env(), ID=getNewID())

  ## add to container
  if(!is.null(container))
    add(container, obj, ...)

  return(obj)
}

## Methods

##' add method for containers
##'
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitQt", obj="gContainerQt",value="RQtObject"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, gqtobject(value))
          })

##' handlers
##' (Don't have toolkit in RQtObject)
setMethod("addhandler",
          signature(obj="RQtObject"),
          function(obj, 
                   signal, handler, action=NULL, ...) {
            .addhandler(gqtobject(obj), force(toolkit), signal, handler, action, ...)
          })

setMethod(".addhandler",
          signature(obj="RQtObject"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addhandler(gqtobject(obj), force(toolkit), signal, handler, action, ...)
          })

setMethod("removehandler", signature("RQtObject"),
          function(obj,  ID=NULL, ...) {
            .removehandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })

setMethod(".removehandler", signature("RQtObject"),
          function(obj, toolkit,  ID=NULL, ...) {
            .removehandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })

setMethod("blockhandler", signature("RQtObject"),
          function(obj, ID=NULL, ...) {
            .blockhandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })
setMethod(".blockhandler", signature("RQtObject"),
          function(obj, toolkit,  ID=NULL, ...) {
            .blockhandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })

setMethod("unblockhandler", signature("RQtObject"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })
setMethod(".unblockhandler", signature("RQtObject"),
          function(obj, toolkit,  ID=NULL, ...) {
            .unblockhandler(gqtobject(obj), guiToolkit("Qt"), ID, ...)
          })


