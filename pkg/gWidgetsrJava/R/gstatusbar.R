## StatusBar. Use value to push message, value to pop
setClass("gStatusbarrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )
## constructor
setMethod(".gstatusbar",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", container=NULL, ...) {

            force(toolkit)            


            statusbar <- .jnew("javax/swing/JLabel", text)
            
            obj = new("gStatusbarrJava",block=statusbar, widget=statusbar,
              toolkit=toolkit, ID=getNewID(),  e = new.env())

            obj@e$label <- text         # avoid gettext for label
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, expand=FALSE)
            }
  
            invisible(obj)
          })

### methods
## right way is to use border layout to add to a container
## ## Add here is used to add to the bottom of the pane
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gWindowrJava", value="gStatusbarrJava"),
          function(obj, toolkit,  value, ...) {
            tag(value, "parentContainer") <- obj
            cont = getWidget(obj)
            sb = getBlock(value)
            pane = cont$getContentPane()
            bl <- .jnew("java/awt/BorderLayout")
            where <- .jnew("java/lang/String", .jfield(bl,name="SOUTH"))

            .jcall(.jcast(pane,"java/awt/Container"),
                   "V",
                   method = "add",
                   .jcast(sb,  "java/awt/Component"),
                   .jcast(where,"java/lang/Object"))
            ## show up without resizing?
            .jcall(obj@widget,"V", "invalidate")
            .jcall(obj@widget,"V", "validate")
          })


## This gets from glabel instance
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gStatusbarrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            return(obj@e$label)
          })

## This pushes to label
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gStatusbarrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   ## two things
                   obj@e$label <- value
                   widget <- getWidget(obj)
                   .jcall(widget, "V",
                          method="setText",
                          as.character(value))
                   return(obj)
                 })

