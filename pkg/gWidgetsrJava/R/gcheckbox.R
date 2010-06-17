setClass("gCheckboxrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## constructor
setMethod(".gcheckbox",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text, checked=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,...) {
            
            force(toolkit)
            
            if(missing(text)) text = ""
            check = .jnew("javax/swing/JCheckBox",
              as.character(text), as.logical(checked))

            obj = new("gCheckboxrJava",block=check, widget=check,
              toolkit=toolkit, ID=getNewID(),  e = new.env())

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            
            if (!is.null(handler)) {
              id = addhandlerchanged(obj, handler, action=action)
            }
  
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            .jcall(.jcast(obj@widget,"javax/swing/AbstractButton"),
                   "Z","isSelected")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(value) {
                     .jcall(.jcast(obj@widget,"javax/swing/AbstractButton"),"V",
                            "doClick")
                   }

                   return(obj)
                 })

## [
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gCheckboxrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            .jcall(.jcast(x@widget,"javax/swing/AbstractButton"),"S",
                   "getText")
          })
            
setMethod("[",
          signature(x="gCheckboxrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gCheckboxrJava"),
          function(x, toolkit, i, j, ..., value) {
            .jcall(.jcast(x@widget,"javax/swing/AbstractButton"),"V",
                   "setText",as.character(value))
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gCheckboxrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

### no method to change the value of text???

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
              class = "java/awt/event/ActionListener", ...)
            return(ID)
            
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gCheckboxrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerchanged(obj, handler, action)
          })
