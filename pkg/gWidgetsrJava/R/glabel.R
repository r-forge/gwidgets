setClass("gLabelrJava",
         contains="gComponentrJava",
         representation = representation("gComponentrJava",
           markup="logical"),

         prototype=prototype(new("gComponentrJava"))
         )

## constructor
setMethod(".glabel",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text= "", markup = FALSE, editable = FALSE, handler = NULL, 
                   action = NULL, container = NULL, 
                   ...
                   ) {

            force(toolkit)
            
            label = .jnew("javax/swing/JButton","")

            label$setBorderPainted(FALSE) # no border
            label$setContentAreaFilled(FALSE) # no shading

            
            obj = new("gLabelrJava",block=label, widget=label,
              toolkit=toolkit,ID=getNewID(), e = new.env(),
              markup=as.logical(markup))


            ## add text with markup possible
            svalue(obj) <- text

            if(editable) {
              handler = function(h,...) {
                w = getWidget(h$obj)

                ## get frame
                frame = w$getTopLevelAncestor()
                
                title="edit label"; message="New value:"; icon="info"
                op = .jnew("gWidgetsrJava/gDialog")
                ans= .jcall(op,"S","gInput",
                  .jcast(frame,"javax/swing/JFrame"),
                  .jnew("java/lang/String",message),
                  .jnew("java/lang/String",svalue(obj)),
                  .jnew("java/lang/String",title),
                  .jnew("java/lang/String",icon)
                  )
                ## set the value unless cancel (returns NULL)
                if(!is.null(ans))
                  svalue(h$obj) <- ans
              }
            }
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler,action=action)
            }
            
            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }
            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLabelrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            markup = obj@markup
            if(is.null(markup)) markup = FALSE

            val = .jcall(obj@widget,"S","getText")

            if(!is.empty(markup) && markup==TRUE)
              val = gsub("<[^>]*>","",val)    # strip off HTML
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLabelrJava"),
          function(obj, toolkit, index=NULL, ..., value) {
            ## set the text
            markup = obj@markup
            if(is.null(markup)) markup = FALSE

            ## is markup working by default here
            if(as.logical(markup)==TRUE)
              .jcall(obj@widget,"V","setText",
                     as.character(paste("<HTML>",value,"</HTML>",sep="",collapse="")))
            else
              .jcall(obj@widget,"V","setText",as.character(value))

            return(obj)
          })


setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLabelrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addJHandler(obj,handler, action,
                        type="addActionListener",
                        event = "ActionEvent",
                        class = "java/awt/event/ActionListener",
                        cast = "javax/swing/AbstractButton",...)

          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gLabelrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

##################################################
## internal function -- used by gvariables in  gcommandline
setGeneric("gaddlabel", function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) standardGeneric("gaddlabel"))

setMethod("gaddlabel",
          signature("guiWidget"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...)
          gaddlabel(obj@widget, text, markup, pos, container, ...)
        )

setMethod("gaddlabel",
          signature("gWidgetrJava"),
          function(obj, text="", markup=FALSE, pos=1, container=NULL, ...) {
            ## wrap widget into a new package with label
            if(pos %in% c(2,4)) {
              group = ggroup(horizontal=TRUE,container=container,
                toolkit=obj@toolkit)
            } else {
              group = ggroup(horizontal=FALSE,container=container,
                toolkit=obj@toolkit)
            }
            
            
            if(pos %in% 2:3) {
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
              add(group, obj,expand=TRUE)
            } else {
              add(group, obj,expand=TRUE)
              glabel(text, markup=markup, container=group, toolkit=obj@toolkit)
            }
            ## group is returned. No methods added here, just a new package
            return(group)
          })
          
