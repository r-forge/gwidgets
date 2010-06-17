## reusuabel chunk of code
setClass("gActionRGtk",
         contains="gComponentRGtk",
         representation(e = "environment"),
         prototype=prototype(e=new.env())
         )


setMethod(".gaction",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   label,
                   tooltip = NULL,
                   icon = NULL,
                   key.accel = NULL,
                   handler = NULL, action = NULL, 
                   ...) {
            
            force(toolkit)

            if(!is.null(icon))
              icon <- getstockiconname(icon)
            
            act <- gtkAction(name = make.names(label),
                             label = label,
                             tooltip = tooltip,
                             stock.id = icon)


            obj = new("gActionRGtk", block=act, widget=act, toolkit=toolkit)

            ## add for later use
            ## should be defined when used in a menu bar.
            tag(obj,"key.accel") <- key.accel
            obj@e$buttons <- list()     # for svalue<- with buttons, menu items work
            
            if(!is.null(handler))
              addHandlerChanged(obj, handler, action)
            
            return(obj)
          })

## svalue -- get label
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            widget <- getWidget(obj)
            return(widget['label'])
          })



## svalue<- set label
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   gtkaction <- getWidget(obj)

                   ## for menu, toolbar est label propoerty
                   gtkaction['label'] <- value

                   ## for buttons, we work harder
                   buttons <- obj@e$buttons
                   if(length(buttons) > 0)
                     sapply(buttons, function(i) {
                       if(isExtant(i))
                         svalue(i) <- value
                     })

                   return(obj)
                 })

## enabled -- inherited
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gActionRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            widget <- getWidget(obj)

            ID <- gSignalConnect(widget, signal="activate",
                           f = handler,
                           data = list(action = action),
                           user.data.first = TRUE)

            invisible(ID)
          })

                             
## helper functions

.isgAction <- function(lst) {
  is(lst,"guiComponent") && is(lst@widget, "gActionRGtk") ||
  is(lst,"gActionRGtk")
}
