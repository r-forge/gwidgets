## reusuabel chunk of code
setClass("gActiontcltk",
         representation(widget="list",e = "environment"),
         prototype(widget=list(), e = new.env())
         )


setMethod(".gaction",
          signature(toolkit="guiWidgetsToolkittcltk"),
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
                        key.accel = key.accel,
                        handler = handler,
                        action = action)

            e <- new.env(); e$buttons <- e$menuitems <- e$toolbaritems <- list()
            e$label <- label
            obj <- new("gActiontcltk", widget = lst, e =e)
            return(obj)
          })

setMethod(".getToolkitWidget",
          signature(obj="gActiontcltk", toolkit="guiWidgetsToolkittcltk"),
          function(obj, toolkit) obj@widget)

## is this a gaction
.isgAction <- function(obj) {
  is(obj,"guiComponent") && is(obj@widget,"gActiontcltk")
}

## methods need to be disabled
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gActiontcltk"),
                 function(obj, toolkit, ..., value) {
                   e <- obj@e
                   if(length(e$buttons) > 0)
                     sapply(e$buttons, function(i) enabled(i) <- as.logical(value))

                   if(length(e$toolbaritems) > 0)
                     sapply(e$toolbaritems, function(i) {
                     if(as.logical(value))
                       tkconfigure(i,state="normal")
                     else
                       tkconfigure(i, state = "disabled")
                   })
                   if(length(e$menuitems) > 0)
                     sapply(e$menuitems, function(i) {
                       if(as.logical(value))
                         tcl(i,"entryconfigure",e$label,state="normal")
                       else
                         tcl(i,"entryconfigure",e$label,state="disabled")
                     })
                   return(obj)
                 })


setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gActiontcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val <- obj@widget$label
            return(val)
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gActiontcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   e <- obj@e
                   if(length(e$buttons) > 0)
                     sapply(e$buttons, function(i) svalue(i) <- as.character(value))

                   if(length(e$toolbaritems) > 0)
                     sapply(e$toolbaritems, function(i) {
                       tkconfigure(i, text=value)
                     })
                   if(length(e$menuitems) > 0)
                     sapply(e$menuitems, function(i) {
                       tcl(i,"entryconfigure",e$label,label=value)
                     })
                   return(obj)
                 })