setClass("gButtonrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


setMethod(".gbutton",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            force(toolkit)
            
            theArgs = list(...)
            
            text = as.character(text)
            allIcons = names(gWidgetsrJavaIcons)
            if(text %in% allIcons)
              iconFile = gWidgetsrJavaIcons[[text, exact=TRUE]]
            else
              iconFile = NULL
            if(!is.null(iconFile)) {
              ## put icon and text
              icon = .jnew("javax/swing/ImageIcon",iconFile)
              button = .jnew("javax/swing/JButton",
                .jnew("java/lang/String",text),.jcast(icon,"javax/swing/Icon"))
            } else {
              button = .jnew("javax/swing/JButton",.jnew("java/lang/String",text))
            }

            ## look like button if border=FALSE
            if(border == FALSE) {
              button$setBorderPainted(FALSE) # no border
              button$setContentAreaFilled(FALSE) # no shading
            }

            obj = new("gButtonrJava",
              block=button, widget=button,
              toolkit=toolkit,ID=getNewID(), e = new.env())

            ## add to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            ## add handler
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
          })


## constructor for gaction instance
## constructor for gaction instance
setMethod(".gbutton",signature(action="guiWidget", toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {
            .gbutton(toolkit, text, border, handler, action = action@widget, container = container, ...)
          })

setMethod(".gbutton",signature(action="gActionrJava", toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", border = TRUE, handler=NULL, action=NULL, container=NULL,...
                   ) {

            alst <- action@widget
            obj <- .gbutton(toolkit,
                            text = alst$label,
                            border = border,
                            handler = alst$handler,
                            action = alst$action,
                            container = container, ...)

            if(!is.null(alst$tooltip))
              .tooltip(obj,toolkit) <- alst$tooltip
            
            action@e$buttons <- c(action@e$buttons,obj)
            return(obj)
          })

### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            .jcall(obj@widget,"S","getText")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   button = obj@widget

                   iconFile = gWidgetsrJavaIcons[[value, exact=TRUE]]
                   if(!is.null(iconFile)) {
                     ## put icon and text
                     icon = .jnew("javax/swing/ImageIcon",iconFile)
                     .jcall(.jcast(button,"javax/swing/AbstractButton"),"V",
                            "setIcon",.jcast(icon,"javax/swing/Icon"))
                     .jcall(button,"V","setText",as.character(value))
                   } else {
                     .jcall(button,"V","setText",as.character(value))
                   }

                   return(obj)
                 })

### handlers
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
                        class = "java/awt/event/ActionListener",
                        cast = "javax/swing/AbstractButton",...)
            return(ID)
          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })

## for popup menu
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gButtonrJava"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            addPopupMenuWithSignal(obj, toolkit, menulist, signal="clicked",...)
})
