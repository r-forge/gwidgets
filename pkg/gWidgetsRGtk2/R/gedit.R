## class defined in aaaClasses for inheritance
## constructor
setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL, 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {
            
            force(toolkit)

            if (is.null(text)) text<-""
  

            entry <- gtkEntryNew()

            obj <- as.gWidgetsRGtk2(entry)

            tag(obj, "coerce.with") <- coerce.with
  
            ## this adds completion fields to this widget. To *add* to the list
            ## of values that can be completed use gEditobject[]<- values
            
            ##  entry$setMaxLength(max(width,length(unlist(strsplit(text,"")))))
            entry$SetText(text)
            tag(obj,"value")  <- text        # store for later
            tag(obj,"completion") <- NULL    # a completion object if set via [<-

            ## width -- ths sets minimum -- it ay expand to fill space
            if(!is.null(width))
              entry$setWidthChars(as.numeric(width))
            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj,...)
            }
            
            if (!is.null(handler)) {
              id = addhandlerchanged(obj,handler,action)
            }
            
            invisible(obj)
            
            
          })


as.gWidgetsRGtk2.GtkEntry <- function(widget, ...) {

  obj = new("gEditRGtk",block=widget, widget=widget,
    toolkit=guiToolkit("RGtk2"))

 ##  ## Drag and drop
##   dropHandler =   function(h,...) {
##     theName = id(h$dropdata)
##     if(is.null(theName)) theName == ""
##     svalue(h$obj) <- ""                 # funny, why isn't this svalue(h$obj)<-theName?
##     ## override value -- in case it is a widget
##     tag(h$obj, "value") <- h$dropdata
##     return(TRUE)
##   }
##   handler.ids = list()
##   id = adddroptarget(obj, targetType="object",handler=dropHandler)
##   handler.ids[['dnd']] = id
  

  return(obj)
}

## code to add  completion to the entry
## only do so if set via [<-
.setCompletion <- function(obj,...) {
  completion = gtkEntryCompletionNew()
  ## set model
  ## this caps out at 1000 -- is this a speed issue?
  model <- rGtkDataFrame(data.frame(character(1000),stringsAsFactors=FALSE))
  completion$SetModel(model)
  completion$SetTextColumn(0)           # Columns count from 0 -- not 1

  ## set properties
  gtktry({completion['inline-completion'] <- TRUE}, silent = TRUE)
  gtktry({completion['inline-selection'] <- TRUE}, silent = TRUE)

  ## set completion
  tag(obj,"completion") <- completion

  ## get entry from obj
  entry <- obj@widget
  entry$SetCompletion(completion)
}


## methods
setMethod("svalue", signature(obj="GtkEntry"),
          function(obj, index=NULL, drop=NULL, ...) {
            .svalue(obj,guiToolkit("RGtk2"), index, drop, ...)
          })
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            val = obj@widget$getText()
            if(!is.null(tag(obj,"coerce.with")))
              val = do.call(tag(obj,"coerce.with"), list(val))

            return(val)
          })
## trouble here -- no coerce.with info available in obj
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="GtkEntry"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...) {
            val = obj$getText()
            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   widget <- getWidget(obj)
                   widget$SetText(value)
                   widget$activate()
                   tag(obj, "value") <- value
                   return(obj)
          })
## want to replace "value" but can't
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="GtkEntry"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   obj$SetText(value)
                   obj$activate()
                   
                   return(obj)
          })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gEditRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            obj <- x
            if(!is.null(tag(obj,"completion"))) {
              store = obj@widget$GetCompletion()$GetModel()
              nrows = dim(store)[1]
              if(missing(i))
                i = 1:nrows
              
              return(store[i , ])
            } else {
              return(c())
            }
          })
            
setMethod("[",
          signature(x="gEditRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            if(missing(i))
              .leftBracket(x,x@toolkit, ...)
            else
              .leftBracket(x,x@toolkit, i, ...)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gEditRGtk"),
          function(x, toolkit, i, j, ..., value) {
            obj <- x
            if(is.null(tag(obj,"completion"))) 
              .setCompletion(obj)

            store = obj@widget$GetCompletion()$GetModel()
            nrows = dim(store)[1]
            n =length(value)
            if(n > nrows)
              values = values[1:nrows]            # truncate
            if(missing(i))
              i = 1:n
            store[i , ]<- value

            ## all done
            return(obj)
          })

setReplaceMethod("[",
                 signature(x="gEditRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##' visible<- if FALSE, for password usage
setReplaceMethod("visible",signature(obj="gEditRGtk"),
          function(obj, ..., value) {
            widget <- getWidget(obj)
            widget$setVisibility(as.logical(value))
            return(obj)
          })


##################################################
## handlers

### doesn't work -- double writes
setMethod(".adddropsource",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ## do nothing, alrady in gedit widget
          })
setMethod(".adddroptarget",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, targetType="text", handler=NULL, action=NULL, ...) {
            ##            gwCat("drop target for gedit uses default only")

            ## issue is if using after=FALSE, the default drophandler is called. (Can't stop signal emission)
            ## if using after=TRUE, the dropped value is put into widget's value in similar way, a
            ## again we don't want this
            ## so we store the pre-value then set after as a hack
            predrophandler <- function(h,...) {
              tag(h$obj,"..predropvalue") <- svalue(h$obj)
            }
            gSignalConnect(getWidget(obj), "drag-data-received", f= predrophandler,
                           data=list(obj=obj), user.data.first=TRUE,
                           after=FALSE)
            
            postdropHandler <- function(h,w, ctxt, x, y, selection, ...) {
              svalue(h$obj) <- tag(h$obj,"..predropvalue") # complete the hack
              tag(h$obj, "..predropvalue") <- NULL
              
              dropdata <- selection$GetText()
              if(is.integer(dropdata)) 
                dropdata <- Paste(intToChar(dropdata))
              else
                dropdata <- rawToChar(dropdata)
              dropdata <- gsub(Paste("^",.gWidgetDropTargetListKey),"", dropdata)
              h$dropdata <- dropdata
              handler(h, widget=w, context=ctxt, x=x, y=y, selection=selection, ...)
            }
            id <- gSignalConnect(getWidget(obj), "drag-data-received", f=postdropHandler,
                           data=list(obj=obj, action=action),
                                 after=TRUE, user.data.first=TRUE)
          })
                               


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            f = function(h,widget,event,...) {
              keyval = event$GetKeyval()
              if(keyval == GDK_Return) {
                handler(h,widget,event,...)
                return(TRUE)
              } else {
                return(FALSE)
              }
            }
#            id = addhandler(obj,signal="key-release-event",handler=f, action=action,...)
            id <- addhandler(obj, signal="activate", handler=handler, action=action)
            return(id)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gEditRGtk"),
          function(obj,toolkit, handler=NULL, action=NULL,...) {
            widget <- getWidget(obj)
            ID <-
              gSignalConnect(widget,signal = "key-release-event",
                             f = function(d,widget,event,...) {
                               h <- list(obj=d$obj,action=d$action)
                               key <- event$GetString()
                               h$key <- key
                               if(!is.null(d$handler) &&
                                  is.function(d$handler))
                                 d$handler(h,...)
                               return(FALSE) # propogate
                             },
                             user.data.first = TRUE,
                             data = list(obj=obj,handler=handler, action=action)
                             )
            invisible(ID)
##            addhandler(obj,"changed",handler,action)
          })

