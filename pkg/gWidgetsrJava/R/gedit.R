## class defined in aaaClasses for inheritance
## constructor
setClass("gEditrJava",
         representation = representation("gComponentrJava",
           coercewith="NULLorFunction"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )


setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL, 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {

           force(toolkit)
            
            if (is.null(text)) text<-""

            ## check that coerce.with is a function
            if(is.null(coerce.with) || is.function(coerce.with)) {
              ## okay
            } else {
              if(is.character(coerce.with)) {
                coerce.with = get(coerce.with)
              }
            }

            
            entry <- .jnew("javax/swing/JTextField",
                           as.character(text),as.integer(width))

           ## fix size issues?
           entry$setMinimumSize(entry$getPreferredSize())
           entry$setMaximumSize(entry$getPreferredSize())
           
            obj = new("gEditrJava",block=entry, widget=entry,
              toolkit=toolkit,ID=getNewID(),  e = new.env(),
              coercewith=coerce.with)



            ## Drag and drop
            adddropsource(obj)
            adddroptarget(obj)

            DEBUG("gedit: add drop handler\n")
##             dropHandler =   function(h,...) {
##               theName = id(h$dropdata)
##               if(is.null(theName)) theName == ""
##               svalue(h$obj) <- ""                 # funny, why isn't this svalue(h$obj)<-theName?
##               ## override value -- in case it is a widget
##               tag(h$obj, "value") <- h$dropdata
##               return(TRUE)
##             }
##             handler.ids = list()
##             id = adddroptarget(obj, targetType="object",handler=dropHandler)
##             handler.ids[['dnd']] = id
            
            
  
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }

            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
            
            
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gEditrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            jobj = getWidget(obj)
            val = jobj$getText()

            coercewith = obj@coercewith
            if(!is.null(coercewith))
              val = do.call(coercewith, list(val))

            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gEditrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   jobj = getWidget(obj)
                   ## use slower reflectance
                   ## setText is JTextComponent class
                   jobj$setText(as.character(value))

                   tag(obj, "value") <- value
                   return(obj)
          })


## left bracket implement completion
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gEditrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            cat("gedit: completion is not implemented\n")
          })
            
setMethod("[",
          signature(x="gEditrJava"),
          function(x, i, j, ..., drop=TRUE) {
            cat("gedit: completion is not implemented\n")
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gEditrJava"),
          function(x, toolkit, i, j, ..., value) {
            cat("gedit: completion is not implemented\n")
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gEditrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##################################################
## handlers

## changed is called after a commit
## keystroke is called when widget display changes


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gEditrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {

            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
              class = "java/awt/event/ActionListener",...)
            return(ID)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gEditrJava"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            jobj = getWidget(obj); jobj = jobj$getDocument()

            ID = addJHandler(obj,handler=handler,action=action,
              type="addDocumentListener",
              event = "insertUpdate",
              class = "javax/swing/event/DocumentListener",
              cast = "javax/swing/text/AbstractDocument",
              jobj = jobj,...
              )
            return(ID)
          })

