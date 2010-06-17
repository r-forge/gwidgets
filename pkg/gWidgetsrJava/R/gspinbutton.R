## Could make spinbutton slider, subclass as methods are identical
setClass("gSpinbuttonrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".gspinbutton",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   from=0,to=10,by=1,value=from,digits=0,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {

            force(toolkit)
            
            spinnerModel = .jnew("javax/swing/SpinnerNumberModel",
              value, from, to , by)
            spinner = .jnew("javax/swing/JSpinner")
            .jcall(spinner,"V","setModel",
                   .jcast(spinnerModel,"javax/swing/SpinnerModel"))

            obj = new("gSpinbuttonrJava", block=spinner, widget=spinner,
              toolkit=toolkit, ID=getNewID(),  e = new.env())

            svalue(obj) <- value                  # wasn't working as desired
  

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            if (!is.null(handler))  {
              id = addhandlerchanged(obj, handler, action)
            }
            invisible(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gSpinbuttonrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            value = .jcall(obj@widget,"Ljava/lang/Object;","getValue")
            return(.jsimplify(value))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gSpinbuttonrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .jcall(obj@widget,"V","setValue",asjobject(value))
                   return(obj)
                 })

### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gSpinbuttonrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addChangeListener",
              event = "ChangeEvent",
              class = "javax/swing/event/ChangeListener",...)
            return(ID)
          })

