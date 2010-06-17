## constructor
setMethod(".gwindow",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   title="Window", visible=TRUE,
                   width = NULL, height = NULL, parent=NULL,
                   handler=NULL, action = NULL,
                   ...
                   ) {

            force(toolkit)
            
            window = .jnew("javax/swing/JFrame",title)
            UIM = .jnew("javax/swing/UIManager")
            UIM$setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
            

            ## set Preferred size.
            ## use size<- to set minimum size
            if(!is.null(width)) {
              if(is.null(height)) height = .7 * width
            } else {
              width <- height <- 200
            }

            d = .jnew("java/awt/Dimension", as.integer(width), as.integer(height))
            .jcall(.jcast(window,"javax/swing/JComponent"),"V","setMinimumSize",d)


            ## set initial location
            location <- parent
            if(!is.null(location)) {
              if (inherits(location,"guiContainer") ||
                 inherits(location,"guiComponent")) {
                try(.jcall(window,"V","setLocationRelativeTo",
                           .jcast(getToolkitWidget(location),"java/awt/Component")))
              } else if(length(location) == 2) {
                location = as.integer(location)
                .jcall(.jcast(window,"java/awt/Component"),"V","setLocation",location[1],location[2])
              }
            }
            ## should be JFrame.EXIT_ON_CLOSE
            ## window$setDefaultLookAndFeelDecorated(TRUE)
            window$setDefaultCloseOperation(as.integer(1))
            
            
            
            obj = new("gWindowrJava",block=window, widget=window,
              toolkit=toolkit, ID=getNewID(),  e = new.env())
            tag(obj,"parentContainer") <- obj
            
            svalue(obj) <- title
            
            if (!is.null(handler)) {
              id <- addhandlerdestroy(obj, handler=handler, action=action)
            }

            if(visible)
              visible(obj) <- visible

            return(obj)
          })
##################################################
## Methods

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava", value="gWidgetrJava"),
          function(obj, toolkit, value, ...) {
            tag(value, "parentContainer") <- obj
            
            .jcall(obj@widget,"Ljava/awt/Component;", "add",
                   .jcast(getBlock(value), "java/awt/Component"))
            .jcall(obj@widget,"V", "invalidate")
            .jcall(obj@widget,"V", "validate")

            .jcall(obj@widget,"V", "pack")
          })


## methods

## svalue refers to title
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            ## return title
            .jcall(obj@widget,"S" , "getTitle")
          })

setMethod(".svalue<-",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, index=NULL,..., value) {
            ## set the title
            .jcall(obj@widget, , "setTitle",as.character(value))
            return(obj)
          })

## no visible() method
setMethod(".visible<-",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ..., value) {
            value = as.logical(value)

            .jcall(obj@widget,,"setVisible", value)
            .jcall(obj@widget,"V", "pack")
            return(obj)
          })


setMethod(".size",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ...) {
            missingMsg(".size,gwindow")
            return()
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, ...) {
            .jcall(obj@widget,,"dispose")
          })


##################################################
## handlers
setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWindowrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="destroy", handler, action, ...)
          })


## helpers

## return top level container or NULL
getTopLevel <- function(obj) {
  ## return NULL or gWidnow object
  parent <- tag(obj,"parentContainer")
  if(is.null(parent) || is(parent,"gWindowrJava"))
    return(parent)
  ## else recurse
  else
    getTopLevel(parent)
}
