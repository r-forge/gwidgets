## can't extend or shorten via []<- but o/w is working
## implement horizontal=TRUE

setClass("gRadiotcltk",
         representation = representation("gComponenttcltk",
           coercewith="NULLorFunction"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## constructor
setMethod(".gradio",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items, selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,       
                   ...
                   ) {
            force(toolkit)

            if(is.data.frame(items))
              items <- items[,1, drop=TRUE]

            n = length(items)

            if (n<2)
              stop(gettext("Radio button group makes sense only with at least two items."))

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning(gettext("Container is not correct. No NULL containers possible\n" ))
              return()
            }

            tt = getWidget(container)
            gp = ttkframe(tt)
            
            theRBs = list(); theLabels = list()
            for(i in 1:n) {
#              theRBs[[i]] = tkradiobutton(gp, anchor="e", text=items[i])
              theRBs[[i]] = ttkradiobutton(gp,  text=items[i])
#              theLabels[[i]] = ttklabel(gp,text=items[i], anchor="w")
            }
            
            theValue = tclVar(items[selected])
            sapply(1:n, function(i)
                   tkconfigure(theRBs[[i]], variable=theValue, value=items[i]))

            if(horizontal) {
              for(i in 1:n) {
                tkpack(theRBs[[i]],side="left", padx = 0)
 #               tkpack(theLabels[[i]],side="left", padx=0)
              }
            } else {
              ## vertical
              for(i in 1:n) {
                tkpack(theRBs[[i]], side="top", anchor="w")
##                tkgrid(theRBs[[i]])#,theLabels[[i]])
##                tkgrid.configure(theRBs[[i]],sticky="e",padx=1)
##              tkgrid.configure(theLabels[[i]],sticky="w")
              }
            }
                      
            ## use coerce with
            theArgs = list(...)
            if(!is.null(theArgs$coerce.with)) {
              coerce.with = theArgs$coerce.with
            } else {
              if(is.numeric(items))
                coerce.with = as.numeric
              else if(is.logical(items))
                coerce.with = as.logical
              else
                coerce.with = as.character
            }
            if(is.character(coerce.with))
              coerce.with = get(coerce.with)
            
            obj = new("gRadiotcltk",block=gp, widget=gp,
              toolkit=toolkit, ID=getNewID(), e = new.env(),
              coercewith = coerce.with)

            tag(obj,"items") <- items
#            tag(obj,"theLabels") <- theLabels
            tag(obj,"theRBs") <- theRBs
            tag(obj,"tclVar") <- theValue
            tag(obj, "..handlers") <- list()
            
            ## add to container
            add(container,  obj,...)
  
            ## add handler
            if(!is.null(handler))
              addhandlerchanged(obj, handler, action)

            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            rbValue = tag(obj,"tclVar")
            rbVal <- as.character(tclvalue(rbValue))

            ## return index or value
            index = ifelse(is.null(index),FALSE,as.logical(index))
            if(index) {
              return(which(as.character(tag(obj,"items")) %in% rbVal))
            } else {
              if(!is.null(obj@coercewith))
                return(obj@coercewith(rbVal))
              else
                return(rbVal)
            }
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   if(is.data.frame(value))
                     value <- value[,1, drop=TRUE]

                   
                   items = obj[]
                   
                   if(!is.null(index) && index==TRUE) {
                     ind = value
                   } else {
                     if(value %in% items) {
                       ind = match(value,items)
                     } else {
                       ind = -1
                     }
                   }
                   if(ind >= 0) {
                     theVar = tag(obj,"tclVar")
                     tclvalue(theVar) <- items[ind]
                   }
                   
                   return(obj)
                 })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gRadiotcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            ## return(items)
            items = tag(x,"items")

            if(missing(i))
              items[,...,drop=drop]
            else
              items[i,...,drop=drop]
          })
            
setMethod("[",
          signature(x="gRadiotcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })


## This sets the labels for the buttons
## add in markup here.
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gRadiotcltk"),
          function(x, toolkit, i, j, ..., value) {

            curVal = svalue(x, index=TRUE)
            n <- length(x)

            ## check
            if(missing(i))
              i = 1:n
            
            if(length(value) != length(i)) {
              cat(gettext("value has the wrong length. Can not alter length using this toolkit\n"))
              return(x)
            }

            ## update items
            items = tag(x,"items")
            items[i] <- value
            tag(x,"items") <- items

            ## set visual labels
            theRBs = tag(x,"theRBs")
            theVar = tag(x,"tclVar")
            for(j in 1:length(i))  {
              tkconfigure(theRBs[[i[j]]], variable=theVar, value=items[i[j]], text=as.character(value[j]))
            }
            tag(x,"theRBs") <- theRBs
            
            ## set the value
            tclvalue(theVar) <- items[curVal]
            ## all done
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gRadiotcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gRadiotcltk"),
          function(x,toolkit) {
            length(tag(x,"items"))
          })

## inherited enabled isn't workgin                
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
                 function(obj, toolkit, ..., value) {

                   theRbs <- tag(obj,"theRBs")
                   sapply(theRbs, function(i) {
                     if(as.logical(value))
                       tcl(i,"state","!disabled")
                     else
                       tcl(i,"state","disabled")
                   })
                   return(obj)
                 })


##################################################
## handlers


## This handler isn't right. It reacts ta click on the box containing
## the widget, not the radio buttons

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {

            l <- tag(obj, "..handlers")
            l$blocked <- FALSE
            if(!is.null(l$handler)) {
              cat(gettext("Can only have one handler set for gradio. Replacing previous"),"\n")
            }
              
            l$handler <- handler
            tag(obj, "..handlers") <- l
            
            theRBs <- tag(obj,"theRBs")
            IDs <- lapply(theRBs, function(i) {
              ## need to pause to let the click catch up
              ## we use scope to look up changeHandler and h
              ## added ButtonRelease as Button-1 wasn't enought with windows
              id <- .addHandler(i,toolkit, signal="<ButtonRelease-1>",
                         actualobj = obj, 
                         action=action,
                         handler = function(h,...) {
                           tcl("after",150,function(...) {
                             l <- tag(obj, "..handlers")
                             if(!getWithDefault(l$blocked, TRUE))
                               l$handler(h,...)
                           }
                          )
                         })
            })
            id <- list(id=1, signal="changed") # XXX only 1 handler
            return(id)
          })

## click and changed the same
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj,toolkit,handler,action,...)
          })


## How to implement these!!!!
setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            tag(obj, "..handlers") <- list()
            invisible()
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            l <- tag(obj, "..handlers")
            l$blocked <- TRUE
            tag(obj, "..handlers") <- l
            invisible()
          })

##' call to unblock a handler by ID. If ID=NULL, all handlers are unblocked
setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gRadiotcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            l <- tag(obj, "..handlers")
            l$blocked <- FALSE
            tag(obj, "..handlers") <- l
            invisible()
          })
