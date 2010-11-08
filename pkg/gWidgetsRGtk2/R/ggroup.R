## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0
            if (horizontal)
              group <- gtkHBoxNew(homogeneous=FALSE, spacing=spacing)
            else
              group <- gtkVBoxNew(homogeneous=FALSE, spacing=spacing)
            
            ## let breath a little
            group$SetBorderWidth(0L)

            
            ## do we pack into a scroll window?
            theArgs = list(...)
            if(use.scrollwindow == TRUE) {
              ## put into a scroll window
              sw = gtkScrolledWindowNew()
              sw$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
              sw$AddWithViewport(group)
              obj = new("gGroupRGtk", block=sw, widget=group, toolkit=toolkit)
            } else {
              obj = new("gGroupRGtk", block=group, widget=group, toolkit=toolkit)
            }
            
            
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container <- gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj, ...)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
              ## we tried Raise and Focus here, but still have bug
              ## with windows causing the drop value to flutter away
              ## after the window is raised. So we cop out and avoid
              ## this on Window

              if(.Platform$OS.type != "windows") {
                ## need drop target before a drag motion!! 
                adddroptarget(obj, handler = function(h,...) {})
                ##              adddropmotion(obj, handler = function(h,...) getWidget(h$obj)$GetWindow()$Raise())
                ## some bug in windows, try focus
                adddropmotion(obj, handler = function(h,...) focus(obj) <- TRUE) ##getWidget(h$obj)$GetParentWindow()$Focus())
              }
            }
            return(obj)
          })

as.gWidgetsRGtk2.GtkVBox <- as.gWidgetsRGtk2.GtkHBox <-
  function(widget,...) {
    obj <- new("gGroupRGtk", block=widget, widget=widget,
               toolkit=guiToolkit("RGtk2"))
  }

##################################################
## methods

## for gGroup
## methods of expand, anchor
## ... arguments: expand, fill, anchor, padding
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk", value="gWidgetRGtk"),
          function(obj, toolkit, value, ...) {
            parent <- getWidget(obj)
            child <- getBlock(value)
            childWidget <- getWidget(value)
            
            theArgs <- list(...)
            
            ## anchor argument
            if(!is.null(theArgs$align))
              theArgs$anchor <- theArgs$align
            if(!is.null(theArgs$anchor)) { ## logic thanks to Felix
              anchor <- theArgs$anchor ## anchor is in [-1,1]^2
              anchor <- (anchor+1)/2      # [0,1]
              anchor[2] <- 1 - anchor[2]     # flip yalign
              ## in gtkstuff
              setXYalign(child, childWidget, anchor)
            }

            ## padding
            if(is.null(theArgs$padding))
              theArgs$padding=0

            ## expand
            expand <- if(is.null(theArgs$expand)) FALSE else theArgs$expand

            fill <- expand
            if(!is.null(theArgs$fill)) {
              if(theArgs$fill == "both") {
                fill <- TRUE
              } else {
                horizontal <- is(obj@widget, "GtkHBox")
                if(theArgs$fill == "x" && horizontal)
                  fill <- TRUE
                else if(theArgs$fill == "y" && !horizontal)
                  fill <- TRUE
              }
            }
            
            parent$packStart(child, expand, fill, theArgs$padding) 
            

            
## This is an example of the pack_start() method.

##   box.pack_start(child, expand, fill, padding)

## box is the box you are packing the object into; the first argument is the child object to be packed. The objects will all be buttons for now, so we'll be packing buttons into boxes.

## The expand argument to pack_start() and pack_end() controls whether the widgets are laid out in the box to fill in all the extra space in the box so the box is expanded to fill the area allotted to it (True); or the box is shrunk to just fit the widgets (False). Setting expand to False will allow you to do right and left justification of your widgets. Otherwise, they will all expand to fit into the box, and the same effect could be achieved by using only one of pack_start() or pack_end().

## The fill argument to the pack methods control whether the extra space is allocated to the objects themselves (True), or as extra padding in the box around these objects (False). It only has an effect if the expand argument is also True.

            
          })


setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk", value="RGtkObject"),
          function(obj, toolkit, value, ...) {

            parent <- getWidget(obj)
            child <- value
            childWidget <- getWidget(value)
            theArgs <- list(...)

            ## anchor argument
            if(!is.null(theArgs$align))
              theArgs$anchor <- theArgs$align
            if(!is.null(theArgs$anchor)) {
              anchor <- theArgs$anchor ## anchor is in [-1,1]^2
              anchor <- (anchor+1)/2      # [0,1]
              anchor[2] <- 1 - anchor[2]     # flip yalign
              ## property
              
              ## in gtkstuff
              setXYalign(child, childWidget, anchor)
            }

            ## expand?
            expand <- if(is.null(theArgs$expand)) FALSE else theArgs$expand

            ## fill argument, only valid whn expand=TRUE

            ## fill only valid when expand is TRUE.
            ## when horizontal=TRUE (left to right, we always fill top top bottom ("y") so only x counts
            ## if horizontal=FALSE, only "y" counts

            fill <- expand
            if(!is.null(theArgs$fill)) {
              if(theArgs$fill == "both") {
                fill <- TRUE
              } else {
                horizontal <- is(obj@widget, "GtkHBox")
                if(theArgs$fill == "x" && horizontal)
                  fill <- TRUE
                else if(theArgs$fill == "y" && !horizontal)
                  fill <- TRUE
              }
            }

            ## pack it in
            parent$packStart(child, expand=expand, fill=fill, padding=0) # expand to fill if TRUE
            
            
          })



setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            getWidget(obj)$SetBorderWidth(as.numeric(value))
            return(obj)
          })


setReplaceMethod(".size",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk"),
          function(obj, toolkit, ...,value) {
            width = value[1]; height = value[2]
            block = obj@block           # use block not widget here in case its a sw
            block$SetSizeRequest(width, height)

            return(obj)
          })

##################################################
## handlers
