## FIX up for non-integer values

setClass("gSliderrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".gslider",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   from=0, to=100, by = 1,
                   value=from,
                   horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container=NULL, ...) {
            force(toolkit)

            ## JSlider is integer only (as far as I can tell
            ## if by < 1, call gspinbutton
            if(by < .99) {
              cat("gslider in rJava is integer only, using gspinbutton instead\n")
              obj = .gspinbutton(toolkit,
                from, to, by, value, digits = 1,
                handler, action,container,...)
              return(obj)
            }
            
            s = .jnew("javax/swing/JSlider")
            if(horizontal)
              scale = .jnew("javax/swing/JSlider",
                .jfield(s,name="HORIZONTAL"),
                as.integer(from),as.integer(to),as.integer(value))
            else
              scale = .jnew("javax/swing/JSlider",
                .jfield(s,name="VERTICAL"),
                as.integer(from),as.integer(to),as.integer(value))

            ## add in properties
            .jcall(scale,"V","setMajorTickSpacing",as.integer((to-from)/4));
            .jcall(scale,"V","setMinorTickSpacing",as.integer((to-from)/8));
            .jcall(scale,"V","setPaintTicks", TRUE);
            .jcall(scale,"V","setPaintLabels", TRUE);

            
            obj = new("gSliderrJava",block=scale, widget=scale,
              toolkit=toolkit, ID=getNewID(),  e = new.env())
            
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
          signature(toolkit="guiWidgetsToolkitrJava",obj="gSliderrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            .jcall(obj@widget,"I","getValue")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gSliderrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .jcall(obj@widget,"V","setValue", as.integer(value))
                   return(obj)
                 })


### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gSliderrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addChangeListener",
              event = "ChangeEvent",
              class = "javax/swing/event/ChangeListener",...)
            return(ID)
          })
