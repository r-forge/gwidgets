## I should make an abstract class for gButton, gImage and gLabel
## instead I get lots of repeated code.


setClass("gImagerJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## image use 


setMethod(".gimage",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   filename = "", dirname="",
                   size="",
                   handler=NULL, action=NULL, 
                   container=NULL, ...) {

            force(toolkit)

            if (size != "") gwCat("gimage: size is currently ignored\n")

            if(is.null(filename))
              filename <- ""
            
            iconFile = NULL
            if(dirname == "stock") {
              iconFile = gWidgetsrJavaIcons[[filename, exact=TRUE]]
            } else if(dirname != "") {
              iconFile = paste(dirname,filename,sep=.Platform$file.sep)
            } else {
              iconFile = filename
            }

            if(!is.null(iconFile) && file.exists(iconFile)) {
              image = .jnew("javax/swing/ImageIcon",iconFile)
            } else {
              ##  uninitialized
              image = .jnull("javax/swing/ImageIcon")
            }

            ## pack into button --
            button = .jnew("javax/swing/JButton")
            .jcall(button,"V","setIcon",
                   .jcast(image,"javax/swing/Icon"))

            ## strip off button look, like glabel()
            button$setBorderPainted(FALSE) # no border
            button$setContentAreaFilled(FALSE) # no shading


            
            obj = new("gImagerJava", block=button, widget=button,
              toolkit=toolkit,ID=getNewID(),  e = new.env()
              )

            tag(obj,"filename") <- iconFile

            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler, action=action)
            }

            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            invisible(obj)
          })
          
### methods
### need to fuss with evb vs. label
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gImagerJava"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ## problem -- we want to add drop target to obj@block evb,
            ## but have handler refer to obj@widgeg=label. 
            addDropTarget(obj@block, toolkit, targetType, handler, action, overrideobj=obj)
            
          })
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gImagerJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            return(tag(obj,"filename"))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gImagerJava"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   ## value is a full filename or icon name
                   if(!file.exists(value)) {
                     ## if not there, look for stock
                     if(!is.null(gWidgetsrJavaIcons[[value, exact=TRUE]])) {
                       value = gWidgetsrJavaIcons[[value, exact=TRUE]]
                     } else {
                       cat("File",value,"does not exist\n")
                       return(obj)
                     }
                   }
                   ## value holds filename, so we are aok
                   newIcon = .jnew("javax/swing/ImageIcon",value)

                   .jcall(obj@widget,"V","setIcon",
                          .jcast(newIcon,"javax/swing/Icon"))
                   .jcall(obj@widget,"V","repaint")
                   
                   ## store dynamically, not with @filename
                   tag(obj,"filename") <- value
                   
                   return(obj)
                 })


### handlers -- same as gButton
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gImagerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addJHandler(obj,handler, action,
                        type="addActionListener",
                        event="ActionEvent",
                        class = "java/awt/event/ActionListener",
                        cast = "javax/swing/AbstractButton",...)

          })
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gImagerJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandlerclicked(obj, handler, action)
          })
