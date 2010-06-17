setClass("gCheckboxtcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## constructor
setMethod(".gcheckbox",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text, checked=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,...) {
            
            force(toolkit)
            
            if(missing(text)) text = ""

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }
            
            tt = getWidget(container)
            gp = ttkframe(tt)

            ## widget
            check = ttkcheckbutton(gp)
            theLabel = ttklabel(gp, text=text)
            ## configure
            tclVar = tclVar(as.numeric(checked))
            tkconfigure(check,variable=tclVar)
            ## layout
            tkgrid(check, theLabel)
            tkgrid.configure(check,stick="e")
            tkgrid.configure(theLabel,stick="w")
            
            obj = new("gCheckboxtcltk",block=gp, widget=check,
              toolkit=toolkit, ID=getNewID(), e = new.env())

            tag(obj,"check") <- check
            tag(obj,"tclVar") <- tclVar
            tag(obj,"label") <- theLabel
            tag(obj,"labelText") <- text
            
            ## add to container
            add(container, obj,...)
            
            if (!is.null(handler)) 
              obj@e$handlerID <- addhandlerchanged(obj, handler, action=action)
  
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            cbVal <- as.logical(as.numeric(tclvalue(tag(obj,"tclVar"))))
            return(cbVal)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   tclvalue(tag(obj,"tclVar")) <-
                     as.character(as.numeric(value))
                   return(obj)
                 })

## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxtcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            theLabel <- tag(x,"labelText")
            return(theLabel)
          })
            
setMethod("[",
          signature(x="gCheckboxtcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxtcltk"),
          function(x, toolkit, i, j, ..., value) {
            tag(x,"labelText") <- as.character(value[1])
            label = tag(x,"label")
            tkconfigure(label, text=as.character(value[1]))
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gCheckboxtcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## inherited enabled isn't workgin                
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
                 function(obj, toolkit, ..., value) {

                   ## change both widget and label
                   sapply(list(tag(obj,"check"), tag(obj,"label")), function(i) {
                   if(as.logical(value))
                     tcl(i,"state","!disabled")
                   else
                     tcl(i,"state","disabled")
                 })
                   
                   return(obj)
                 })

### no method to change the value of text???

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            changeHandler <- handler

            theArgs <- list(...); actualobj <- theArgs$actualobj
            if(is.null(actualobj))
              actualobj <- obj
            
             addhandler(obj,toolkit, signal="<Button-1>",
                        action=action, actualobj=actualobj,
                        handler = function(h,...) {
                            tcl("after",150,function(...) {
                              changeHandler(h,...) ## need to pause
                            })
                          })
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxtcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action)
          })
