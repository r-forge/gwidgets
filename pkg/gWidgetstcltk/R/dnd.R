## idea from http://wiki.tcl.tk/416

## globals within NAMESPACE
.dragging <- FALSE; .dragValue = ""; .lastWidgetID <- ""
dnd.env <- new.env()
dnd.env[['dragging']] <- NULL
dnd.env[['dragValue']] <- ""
dnd.env[['lastWidgetID']] <- ""


##assignInNamespace(".dragging", FALSE,"gWidgetstcltk")
##assignInNamespace(".dragValue", "","gWidgetstcltk")
##assignInNamespace(".lastWidgetID", "","gWidgetstcltk")


##################################################
##
## function used by tcltkObject and gWidgettcltk
addDropSource = function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
  widget = getWidget(obj)

  
  tkbind(widget,"<ButtonPress-1>",function(w,...) {
    dnd.env[['dragging']] <- TRUE
    dnd.env[['lastWidgetID']] <- widget
##    assignInNamespace(".dragging", TRUE,"gWidgetstcltk")
##    assignInNamespace(".lastWidgetID", widget,"gWidgetstcltk")    
    h = list();
    h$obj = obj;
    h$action=action
    if(is.null(handler))
      handler = function(h,...) svalue(obj) # default handler
    dnd.env[['dragValue']] <- handler(h)
    ## assignInNamespace(".dragValue", handler(h),"gWidgetstcltk")
  })
  
  tkbind(widget,"<Motion>",function(x,y,...) {
    .dragging <- dnd.env[['dragging']] ##= getFromNamespace(".dragging","gWidgetstcltk")
    .lastWidgetID <- dnd.env[['lastWidgetID']] ##= getFromNamespace(".lastWidgetID","gWidgetstcltk")

    if(!.dragging) return()
    x0 = as.integer(tkwinfo("rootx",widget))
    y0 = as.integer(tkwinfo("rooty",widget))
    w = tkwinfo("containing",x0+as.integer(x),y0+as.integer(y))
    
    
    if(as.logical(tkwinfo("exists",w)) &&
       length(as.character(w)) > 0 &&
       length(as.character(.lastWidgetID)) > 0
       ) {
      if(as.character(w)[1] != as.character(.lastWidgetID)[1]) {
        tkevent.generate(.lastWidgetID,"<<DragLeave>>")
      }
    }
    dnd.env[['lastWidgetID']] <- ""
    ##    assignInNamespace(".lastWidgetID", "","gWidgetstcltk")    

    if(as.logical(tkwinfo("exists",w)))
      tkevent.generate(w, "<<DragOver>>")
    ## cursor list at
    ##http://developer.apple.com/documentation/Darwin/Reference/ManPages/mann/cursors.ntcl.html#//apple_ref/doc/man/n/cursors
    tkconfigure(widget,cursor="target")
  })
  
  tkbind(widget,"<ButtonRelease-1>",function(x,y,...) {
    .dragging <- dnd.env[['dragging']] ##= getFromNamespace(".dragging","gWidgetstcltk")
    if(!.dragging) return()
    x0 = as.integer(tkwinfo("rootx",widget))
    y0 = as.integer(tkwinfo("rooty",widget))
    w = tkwinfo("containing",x0+as.integer(x), y0+as.integer(y))
    
    if(as.logical(tkwinfo("exists", w))) {
      tkevent.generate(w,"<<DragLeave>>")
      tkevent.generate(w,"<<DragDrop>>")
      tkconfigure(w,cursor="")
    }
    dnd.evn[['dragging']] <- FALSE
##    assignInNamespace(".dragging",FALSE,"gWidgetstcltk")
    tkconfigure(widget,cursor="")
  })
}


setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            addDropSource(obj, toolkit, targetType, handler, action, ...)
          })


setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, targetType="text",
                   handler=NULL, action=NULL, ...) {
            addDropSource(obj, toolkit, targetType, handler, action, ...)
          })

## motino
setMethod(".adddropmotion",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,  handler=NULL, action=NULL, ...) {
            .addHandler(obj,toolkit,signal="<<DragOver>>",handler,action,...)
          })
setMethod(".adddropmotion",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit,  handler=NULL, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="<<DragOver>>",handler, action, ...)
          })


##################################################
## target -- how to add for tcltkObjects?
addDropTarget = function(obj, toolkit, targetType="text", handler=NULL, action=NULL,
  overrideobj = NULL,...) {

  widget = getWidget(obj)
  
  if(is.null(handler))
    handler = function(h,...) svalue(obj) <- h$dropdata

  ## bind to three events
  
  tkbind(widget,"<<DragOver>>",function(w,...) {
    .dragging <- dnd.env[['dragging']] ## getFromNamespace(".dragging","gWidgetstcltk")
    if(.dragging) {
      ##      tkconfigure(widget,relief="raised")
    }
    
    tkbind(widget,"<<DragLeave>>",function(w,...) {
      .dragging <- dnd.env[['dragging']] ##= getFromNamespace(".dragging","gWidgetstcltk")
      if(.dragging) {
        tkconfigure(widget, cursor="")
      }
      ##    tkconfigure(widget,relief="flat")
    })
    tkbind(widget,"<<DragDrop>>",function(w,...) {
      h = list()
      h$obj = obj; h$action=action
      h$dropdata <- dnd.env[['dragValue']] ##getFromNamespace(".dragValue","gWidgetstcltk")
      svalue(obj) <- h$dropdata
      handler(h)
      dnd.env[['dragValue']] <- ""
      ##      assignInNamespace(".dragValue", "","gWidgetstcltk")
    })
  })
}
         
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            addDropTarget(obj, toolkit, targetType, handler, action, ...)
          })
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            addDropTarget(obj, toolkit, targetType, handler, action, ...)
          })
            
