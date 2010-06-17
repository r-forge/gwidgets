setClass("gHtmlrJava",
         contains="gComponentrJava",
         representation = representation("gComponentrJava"),
         prototype=prototype(new("gComponentrJava"))
         )

## helper
.isURL <- function(x)  length(grep("^(ftp|http|file)://", x)) >0
            

## constructor
setMethod(".ghtml",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   x,                   # text or url
                   handler = NULL, 
                   action = NULL, container = NULL, 
                   ...
                   ) {

            force(toolkit)

            if(.isURL(x)) {
              gwCat("XXX fill me in for urls")
              x <- "urls not supported yet"
            }
            
            h <- .jnew("javax/swing/JLabel", x)
            
            obj = new("gHtmlrJava",block=h, widget=h,
              toolkit=toolkit,ID=getNewID(), e = new.env())

            ## save label value
            obj@e$label <- x
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler,action=action)
            }
            
            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }
            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gHtmlrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ..) {
            val <- obj@e$label
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gHtmlrJava"),
          function(obj, toolkit, index=NULL, ..., value) {
            if(.isURL(value)) {
              gwCat("XXX can't do this yet")
              value <- "URLs not supported"
            }

            ## save
            obj@e$label <- value

            ## paste into html tags
            value <- paste("<html>", value, "</html>", sep="", collapse="")
            
            .jcall(obj@widget,"V","setText",value)

            return(obj)
          })


## setMethod(".addhandlerclicked",
##           signature(toolkit="guiWidgetsToolkitrJava",obj="gHtmlrJava"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             addJHandler(obj,handler, action,
##                         type="addActionListener",
##                         event = "ActionEvent",
##                         class = "java/awt/event/ActionListener",
##                         cast = "javax/swing/AbstractButton")

##           })
## setMethod(".addhandlerchanged",
##           signature(toolkit="guiWidgetsToolkitrJava",obj="gHtmlrJava"),
##           function(obj, toolkit, handler, action=NULL, ...) {
##             addhandlerclicked(obj, handler, action)
##           })

