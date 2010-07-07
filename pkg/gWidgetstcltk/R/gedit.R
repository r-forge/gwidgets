## class defined in aaaClasses for inheritance
## constructor
setClass("gEdittcltk",
         representation = representation("gComponenttcltk",
           coercewith="NULLorFunction"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )


setMethod(".gedit",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   text="", width=25,
                   coerce.with = NULL, 
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...
                   ) {

           force(toolkit)
            
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }


           if (is.null(text)) text<-""

            ## check that coerce.with is a function
            if(is.null(coerce.with) || is.function(coerce.with)) {
              ## okay
            } else {
              if(is.character(coerce.with)) {
                coerce.with = get(coerce.with)
              }
            }

           tt <- getWidget(container)

           entryValue = tclVar("")
           entry = ttkentry(tt, width=as.character(width),
             textvariable=entryValue)

           ## what gives with this, was causing an error. No reason to manage  here
           ##           tkgrid(entry)
           
            obj <- new("gEdittcltk",block=entry, widget=entry,
              toolkit=toolkit,ID=getNewID(), e = new.env(),
              coercewith=coerce.with)
           tag(obj,"tclVar") <- entryValue

           ## set the initial text
           svalue(obj) <- text
           

           ## type ahead support
           tag(obj,"typeAhead") <- c()
           tkbind(entry, "<KeyRelease>", function(W, K) {
             if(K == "BackSpace")
               return()
             eVar <- tag(obj,"tclVar")
             x <- obj[]
             cur <- tclvalue(eVar)
             ind <- which(cur == substr(x, 1, nchar(cur)))
             if(length(ind) == 1) {
               ## replace
               tclvalue(eVar) <- x[ind]
               ## set selection
               tcl(W,"selection","range", nchar(cur), nchar(x[ind]))
             }
           })
           

           ## Drag and drop
           ## addDropSource(obj)
           ## addDropTarget(obj)
           
           add(container, obj,...)
           
           if (!is.null(handler)) 
             tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)
           
           
           invisible(obj)
            
            
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            val = tclvalue(tag(obj,"tclVar"))
            if(val == "<NA>")
              val <- NA
            coercewith = obj@coercewith
            if(!is.null(coercewith))
              val = do.call(coercewith, list(val))

            return(val)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   if(is.na(value))
                     value <- "<NA>"
                   
                   tclvalue(tag(obj, "tclVar")) <- value
                   return(obj)
          })


## left bracket implement completion
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gEdittcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            vals <- tag(x, "typeAhead")
            if(missing(i))
              vals
            else
              vals[i]
          })
            
setMethod("[",
          signature(x="gEdittcltk"),
          function(x, i, j, ..., drop=TRUE) {
            if(missing(i))
              .leftBracket(x,x@toolkit, ...)
            else
              .leftBracket(x,x@toolkit, i, ...)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gEdittcltk"),
          function(x, toolkit, i, j, ..., value) {
            vals <- tag(x, "typeAhead")
            if(missing(i))
              vals <- value
            else
              vals[i] <- value
            tag(x, "typeAhead") <- vals
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gEdittcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })


setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
                 function(obj, toolkit, ..., value) {
                   if(is.numeric(value))
                     tkconfigure(obj@widget,width=floor(value[1]/5)) 
                                        # convert pixels to chars
                   else
                     cat(gettext("size needs a numeric vector c(width,...)\n"))
                   return(obj)
                 })


##' visible<- if FALSE, for password usage
setReplaceMethod(".visible",signature(toolkit="guiWidgetsToolkittcltk", obj="gEdittcltk"),
          function(obj, toolkit, ..., value) {
            widget <- getWidget(obj)
            if(as.logical(value))
              tkconfigure(widget, show="")
            else
              tkconfigure(widget, show="*")
            return(obj)
          })


##################################################
## handlers

## changed is called after a commit
## keystroke is called when widget display changes


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gEdittcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="<Return>", handler, action)
          })

