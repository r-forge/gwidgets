## TODO
## * FONTS

setClass("gTextrJava",
         representation(tags="list"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".gtext",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text=NULL,
                   width=NULL, height=300,
                   font.attr = NULL, wrap = TRUE,
                   handler = NULL, action=NULL,
                   container=NULL, ...) {


            force(toolkit)
            
            ## the text area
            textview = .jnew("javax/swing/JTextArea")

            
            ## line wrap
            .jcall(textview,,"setLineWrap",as.logical(wrap))
            .jcall(textview,"V","setWrapStyleWord",as.logical(TRUE))


            ## the scrollpane
            sp = .jnew("javax/swing/JScrollPane", as.jcomponent(textview))
            
            ## set scrollpane properties
            ## size
            if(is.null(width)) width = 200
            d = .jnew("java/awt/Dimension")
            d$setSize(width,height)
            .jcall(as.jcomponent(sp),"V","setPreferredSize",d)

            ## ... fix me
            ## add to scrollpane
#            .jcall(sp,"Ljava/awt/Component;","add",as.jcomponent(textview))
            
            obj = new("gTextrJava", block=sp, widget=textview, tags=list(),
              toolkit=toolkit,ID=getNewID(),  e = new.env())


            ## add initial text
            if(!is.null(text)) {
              add(obj, text, font.attr=font.attr)
            }
            
            adddropsource(obj)
            adddroptarget(obj)

            
            ## attach to container
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }

            ## add handler
            if (!is.null(handler)) {
              id = addhandler(obj, "changed", handler, action)
            }
            return(obj)
          })

## drop=TRUE to get only mouse selected text
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## grab all text
            buffer = obj@widget
            if(is.null(drop) || drop == FALSE) {
              val = buffer$getText()
            } else {
              val = buffer$getSelectedText()
            }
            if(is.null(val)) val <- ""
            return(val)
            })
          
##  svalue<-() replaces text
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   buffer = obj@widget
                   if(length(value) > 1)
                     value = paste(value, collapse="\n")
                   buffer$setText(value)
                   return(obj)
                 })


## clear all text in buffer
setMethod("dispose",signature(obj="gTextrJava"),
          function(obj,...)  {
            .dispose(obj, obj@toolkit, ...)
          })
setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava"),
          function(obj, toolkit,  ...) {
            svalue(obj) <- ""
          })


### Add method is a workhorse for this class. Value can be
## * a line of text
## * a vector of lines of text
## need to do where value of "point"
## add, as a method, needs to have a consistent signature. I'

## add text
setMethod(".insert",
          signature(toolkit="guiWidgetsToolkitrJava",obj = "gTextrJava"),
          function(obj, toolkit, value, where = c("end","beginning","at.cursor"),
                   font.attr = NULL,
                   do.newline = TRUE, ...) {
            ## just call add
            where = match.arg(where)
            .add(obj, toolkit, value, where=where, font.attr=font.attr,
                 do.newline=do.newline, ...)
          })
## add does all the work

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava",value="character"),
          function(obj, toolkit, value,  ...) {
            theArgs = list(...)                      # look for font.attr, do.newline, where

            do.newline = ifelse(is.null(theArgs$do.newline), TRUE, as.logical(theArgs$do.newline))
            markup = theArgs$font.attr
            if(!is.null(markup))
              markup = markup[markup %in% unlist(obj@tags)] # only some markup
            where = ifelse(is.null(theArgs$where), "end",theArgs$where)

            buffer = obj@widget


            for(i in 1:length(value) ) {
              if(is.null(markup)) {
                buffer$append(value[i])
              } else {
                ## do markup here
                fontObj = .jnew("java/awt/Font","Serif",
                  as.integer(0),as.integer(16)) # for constants fontObj$ITALIC

                
                buffer$append(value[i])
              }
              if(do.newline)
                buffer$append("\n")
            }

            ## now place cursor at end of buffer!
            maxValue = .jnew("java/lang/Integer",as.integer(0))
            maxValue = .jfield(maxValue,name="MAX_VALUE")
            sp = obj@block
            sp$getVerticalScrollBar()$setValue(maxValue);
          })

## add a widget
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava",value="guiWidget"),
          function(obj, toolkit, value,  ...) {
            .add(obj,toolkit, value@widget, ...)
          })

setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava",value="gWidgetrJava"),
          function(obj, toolkit, value,  ...) {

            cat("gtext: implement adding a widget to text area\n")
            return()
            })


## set the font for the selected area of the gtext object
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava"),
                 function(obj, toolkit, ..., value) {
                   gwCat("gtext: implement font()\n")
                   return(obj)
                 })


## how to get the keystroke from Java back into R?
setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gTextrJava"),
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

