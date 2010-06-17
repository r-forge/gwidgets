## expander group, like a group, only expands, contracts if requested
## inherits from ggroup, see ggroup's arguments: horizontal, spacing, container
setClass("gExpandgrouprJava",
         contains="gContainerrJava",
         prototype=prototype(new("gContainerrJava"))
         )


setMethod(".gexpandgroup",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", markup=FALSE, horizontal=TRUE,
                   handler=NULL, action=NULL,
                   container = NULL, ...){

            force(toolkit)
            
            iconDir = paste(system.file("images",package="gWidgetsrJava"),
              .Platform$file.sep,sep="")
            exg = .jnew("gWidgetsrJava/gExpandGroup",text, iconDir )
            cont = exg$returnContainer()
            group = ggroup(horizontal=horizontal, ...)
            .jcall(exg,"V","addComponent",
                   .jcast(getWidget(group),"java/awt/Component"))  

            obj = new("gExpandgrouprJava",block = cont, widget = group,
              toolkit = toolkit, ID = getNewID(),  e = new.env())

            tag(obj, "expandGroup") <- exg

            if(!is.null(handler)) {
              cat("No expandhandler available\n");
            }

            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj)
            }

            invisible(obj)
          })



## methods
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava", value = "gWidgetrJava"),
          function(obj, toolkit, value,  ...) {
            ## add value to expandgroup
            add(obj@widget, value, ...)
          })
setMethod(".addSpace",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
          function(obj, toolkit, value,  ...) {
            ## add value to expandgroup
            addSpace(obj@widget, value, ...)
          })
setMethod(".addSpring",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
          function(obj, toolkit,  ...) {
            ## add value to expandgroup
            addSpring(obj@widget, ...)
          })


## value refers to label
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            .jcall(tag(obj,"expandGroup"),"S","getValue")
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   .jcall(tag(obj,"expandGroup"),"S","setValue",
                          as.character(value))
                   return(obj)
                 })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
          function(obj, toolkit, set=TRUE,...) {
            tag(obj,"expandGroup")$getVisibility()
          })

## control expand/close with logical
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
                 function(obj, toolkit, ..., value) {
                   exg = tag(obj,"expandGroup")
                   .jcall(exg,"V","setVisibility",
                          .jnew("java/lang/Boolean",as.logical(value)))

                   return(obj)
                 })


## handlers
## putonto expander button
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gExpandgrouprJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            cat("addhandlerchanged Not implemented\n")
          })
