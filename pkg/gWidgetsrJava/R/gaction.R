## reusuabel chunk of code
## not really implemented 
setClass("gActionrJava",
         representation(widget="list", e="environment"),
         prototype(widget=list(), e=new.env())
         )


setMethod(".gaction",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   label,
                   tooltip = NULL,
                   icon = NULL,
                   key.accel = NULL, 
                   handler = NULL, action = NULL, 
                   ...) {
            
            force(toolkit)

            lst <- list(label = label,
                        tooltip = tooltip,
                        icon = icon,
                        key.accel = NULL,
                        handler = handler,
                        action = action)
            
            e <- new.env(); e$buttons <- e$menuitems <- e$toolbaritems <- list()
            e$label <- label
            
            obj <- new("gActionrJava", widget = lst, e = e)
            
            return(obj)
          })

setMethod(".getToolkitWidget",
          signature(obj="gActionrJava", toolkit="guiWidgetsToolkitrJava"),
          function(obj, toolkit) obj@widget)

## is this a gaction
.isgAction <- function(obj) {
  is(obj,"guiComponent") && is(obj@widget,"gActionrJava")
}


## methods need to be disabled
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gActionrJava"),
                 function(obj, toolkit, ..., value) {
                   e <- obj@e
                   if(length(e$buttons) > 0) {
                     sapply(e$buttons, function(i) enabled(i) <- as.logical(value))
                   }

                   if(length(e$toolbaritems) > 0)
                     sapply(e$toolbaritems, function(i) {
                       ## configure java toolbar item
                       .jcall(i,"V", "setEnabled",as.logical(value))
                     })
                   if(length(e$menuitems) > 0)
                     sapply(e$menuitems, function(i) {
                       ## configure java menubar item
                       .jcall(i,"V", "setEnabled",as.logical(value))
                     })
                   return(obj)
                   
                 })


setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gActionrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val <- obj@widget$label
            return(val)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gActionrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   e <- obj@e
                   if(length(e$buttons) > 0) {
                     sapply(e$buttons, function(i) svalue(i) <- value)
                   }

                   if(length(e$toolbaritems) > 0)
                     sapply(e$toolbaritems, function(i) {
                       ## configure java toolbar item
                       .jcall(.jcast(i,"javax/swing/AbstractButton"),"V",
                              "setText",
                              .jnew("java/lang/String",as.character(value))
                              )
                     })
                     if(length(e$menuitems) > 0)
                       sapply(e$menuitems, function(i) {
                         ## configure java menubar item
                         .jcall(.jcast(i,"javax/swing/AbstractButton"),"V",
                                "setText",
                                .jnew("java/lang/String",as.character(value))
                                )
                       })

                     return(obj)
                 })
