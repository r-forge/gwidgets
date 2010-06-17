

##################################################
##
## function used by rJavaObject and gWidgetrJava
addDropSource = function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
  jobj = getWidget(obj)
  x = try(jobj$setDragEnabled(TRUE), silent=TRUE)
  if(inherits(x,"try-error"))
    cat(x,"\n")
}


setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            addDropSource(obj, toolkit, targetType, handler, action, ...)
          })


setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            addDropSource(obj, toolkit, targetType, handler, action, ...)
          })

## motino
setMethod(".adddropmotion",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,  handler=NULL, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="drag-motion",handler, action, ...)
          })
setMethod(".adddropmotion",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit,  handler=NULL, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="drag-motion",handler, action, ...)
          })


##################################################
## target -- how to add for rJavaObjects?
addDropTarget = function(obj, toolkit, targetType="text", handler=NULL, action=NULL,
  overrideobj = NULL,...) {

  jobj = getWidget(obj)
  try(jobj$setDragEnabled(TRUE),silent=TRUE)
  
}

setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            addDropTarget(obj, toolkit, targetType, handler, action, ...)
          })
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            addDropTarget(obj, toolkit, targetType, handler, action, ...)
          })
            
