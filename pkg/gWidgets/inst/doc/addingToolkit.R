###################################################
### chunk number 1: 
###################################################
  ## load toolkit 
  options("guiToolkit"=NA)
  library(gWidgets)
  library(tcltk)


###################################################
### chunk number 2: 
###################################################
  ## some classes
  setClass("gWidgetTcltk")
  ## A virtual class to hold tcltk object or guiWidget or gWidgetTcltk
  setClass("guiWidgetORgWidgetTcltkORtcltk")
  setIs("guiWidget","guiWidgetORgWidgetTcltkORtcltk")
  setIs("gWidgetTcltk","guiWidgetORgWidgetTcltkORtcltk")


###################################################
### chunk number 3: 
###################################################
  oldclasses = c("tkwin")
  for(i in oldclasses) {
    setOldClass(i)
    setIs(i,"guiWidgetORgWidgetTcltkORtcltk")
  }


###################################################
### chunk number 4: 
###################################################
### Make some base classes
setClass("gComponentTcltk",
representation(
widget="guiWidgetORgWidgetTcltkORtcltk",
toolkit="guiWidgetsToolkit"
),
contains="gWidgetTcltk",
)
setClass("gContainerTcltk",
representation(
widget="guiWidgetORgWidgetTcltkORtcltk",
toolkit="guiWidgetsToolkit"
),
contains="gWidgetTcltk",
)


###################################################
### chunk number 5: 
###################################################
## top level window
setClass("gWindowTcltk",
contains="gContainerTcltk",
prototype=prototype(new("gContainerTcltk"))
)


###################################################
### chunk number 6: 
###################################################
  setMethod(".gwindow",
  signature(toolkit="guiWidgetsToolkittcltk"),
  function(toolkit,
  title="Window", visible=TRUE,
  handler=NULL, action = NULL,
  ...
  ) {
    win <- tktoplevel()
    tktitle(win) <- title
    
    obj = new("gWindowTcltk", widget=win, toolkit=toolkit)
    return(obj)
  })


###################################################
### chunk number 7: 
###################################################
setMethod(".svalue",
signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowTcltk"),
function(obj, toolkit, index=NULL, drop=NULL, ..) {
  tktitle(obj@widget)
})

setMethod(".svalue<-",
signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowTcltk"),
function(obj, toolkit, index=NULL,..., value) {
  ## set the title
  tktitle(obj@widget) <- value
  return(obj)
})


###################################################
### chunk number 8: 
###################################################
setMethod(".add",
signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowTcltk",
value="guiWidget"),
function(obj, toolkit, value, ...) {
  ## how to add?
  tkpack(value@widget@widget)
})


###################################################
### chunk number 9: 
###################################################
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWindowTcltk"),
          function(obj, toolkit, ...) {
            tkdestroy(obj@widget)
          })


###################################################
### chunk number 10: 
###################################################
###########
## label class
setClass("gLabelTcltk",
contains="gComponentTcltk",
prototype=prototype(new("gComponentTcltk"))
)


###################################################
### chunk number 11: 
###################################################
  ## constructor
  setMethod(".glabel",
  signature(toolkit="guiWidgetsToolkittcltk"),
  function(toolkit,
  text= "", markup = FALSE, editable = FALSE, handler = NULL, 
  action = NULL, container = NULL, 
  ...
  ) {
    
    ## if container is non null, we can evaluate expression
    if(is.null(container)) {
      cat("Can't have an NULL container with tcltk")
    }
    ## find tk container
    if(is(container,"guiWidget")) container=container@widget
    if(is(container,"gContainerTcltk")) container=container@widget
    
    label = tklabel(container, text=text)
    
    obj = new("gLabelTcltk",widget=label, toolkit=toolkit)
    
    ## pack into container
    tkpack(label)
    
    ## add callback
    ## no callbacks for labels
    return(obj)
  })


###################################################
### chunk number 12: 
###################################################
  setMethod(".svalue",
  signature(toolkit="guiWidgetsToolkittcltk",obj="gLabelTcltk"),
  function(obj, toolkit, index=NULL, drop=NULL, ..) {
    cat("How to retrieve label text\n")
  })
  
  ## svalue<-
  setReplaceMethod(".svalue",
  signature(toolkit="guiWidgetsToolkittcltk",obj="gLabelTcltk"),
  function(obj, toolkit, index=NULL, ..., value) {
    ## set the text
    tkconfigure(obj@widget, text=value)
    return(obj)
  })


###################################################
### chunk number 13: 
###################################################
  ### button class
  setClass("gButtonTcltk",
  contains="gComponentTcltk",
  prototype=prototype(new("gComponentTcltk"))
  )


###################################################
### chunk number 14: 
###################################################
  setMethod(".gbutton",
  signature(toolkit="guiWidgetsToolkittcltk"),
  function(toolkit,
  text="", handler=NULL, action=NULL, container=NULL,...
  ) {
    
    if(!is.null(container)) {
      topwin = container@widget@widget
    } else {
      topwin = gwindow(toolkit=toolkit)@widget
    }
    
    button = tkbutton(topwin, text=text)
    
    obj = new("gButtonTcltk",widget=button, toolkit=toolkit)
  
    tkpack(obj@widget)
    
    if(!is.null(handler))
    .addhandlerclicked(obj, toolkit, handler=handler)
    
    return(obj)
    
  })


###################################################
### chunk number 15: 
###################################################
setMethod("addhandlerclicked",signature(obj="gWidgetTcltk"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, obj@toolkit,handler, action, ...)
          })


###################################################
### chunk number 16: 
###################################################
  setReplaceMethod(".svalue",
  signature(toolkit="guiWidgetsToolkittcltk",obj="gButtonTcltk"),
  function(obj, toolkit, index=NULL, ..., value) {
    
    tkconfigure(obj@widget, text=value)
    return(obj)
  })


###################################################
### chunk number 17: 
###################################################
## add handler for click event. This is good just for button, label
setMethod(".addhandlerclicked",
signature(toolkit="guiWidgetsToolkittcltk",obj="gButtonTcltk"),
function(obj, toolkit, handler, action=NULL, ...) {
  ## can't handle arguments to handler
  tkconfigure(obj@widget, command=handler)
})


###################################################
### chunk number 18: 
###################################################
  guitoolkit = new("guiWidgetsToolkittcltk")
  win = gwindow("Hello world", toolkit=guitoolkit)
  label=glabel("Hello world, how are you?", container=win, toolkit=guitoolkit)
  button=gbutton("Close", handler=function(h,...) dispose(win),
  container=win, toolkit=guitoolkit)


