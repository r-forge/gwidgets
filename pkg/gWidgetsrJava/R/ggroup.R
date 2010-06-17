## class in aaaClasses.R
## constructor
setMethod(".ggroup",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   horizontal = TRUE, spacing = 5,
                   use.scrollwindow = FALSE, 
                   container = NULL, ... 
                   ) {

            force(toolkit)
            
            theArgs = list(...)                   # raise.on.dragmotion
            
            if(is.null(spacing))
              spacing = 0


            if (horizontal) {
              group = .jnew("javax/swing/Box",as.integer(0))
              group$createHorizontalBox()
            } else {
              group = .jnew("javax/swing/Box",as.integer(1))
              group$createVerticalBox()
            }
            
            ## let breath a little
            ##group$SetBorderWidth(2)
            if(use.scrollwindow == TRUE) {
              ## the scrollpane
              sp = .jnew("javax/swing/JScrollPane", as.jcomponent(group))
              ## set scrollpane properties
              if(is.null(theArgs$width)) theArgs$width = 300
              if(is.null(theArgs$height)) theArgs$height = 300
              d = .jnew("java/awt/Dimension")
              d$setSize(theArgs$width,theArgs$height)
              .jcall(as.jcomponent(sp),"V","setPreferredSize",d)
              obj = new("gGrouprJava", block=sp, widget=group,
                toolkit=toolkit, ID=getNewID(),  e = new.env(),
                horizontal=horizontal)
            } else {
              obj = new("gGrouprJava", block=group, widget=group,
                toolkit=toolkit, ID=getNewID(),  e = new.env(),
                horizontal=horizontal)
            }
            
            ## attach to container if there
            if(!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=toolkit)
              add(container, obj, ...)
            }

            ## raise if we drag across
            if(!is.null(theArgs$raise.on.dragmotion)) {
              DEBUG("ggroup: need to fix raise on dragmotion\n")
              adddroptarget(obj, handler = function(h,...) {})
              adddropmotion(obj, handler = function(h,...) getWidget(h$obj)$GetWindow()$Raise())
            }
            return(obj)
          })


##################################################
## methods



setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGrouprJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..., value) {
            ## adds some breathing room to object
            ## value is pixels
            DEBUG("ggroup: implement svalue<-")

            return(obj)
          })

##################################################
## handlers
