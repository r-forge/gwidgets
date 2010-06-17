## build widget based on gcheckbox
setClass("gCheckboxgrouptcltk",
         representation = representation("gComponenttcltk",
           coercewith="NULLorFunction"),
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

setMethod(".gcheckboxgroup",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   items, checked = FALSE,
                   horizontal=FALSE, use.table=TRUE,
                   handler = NULL, action = NULL, container = NULL, ...) {

            force(toolkit)
            
            if(missing(items) || length(items) == 0)
              stop("Need items to be a vector of items")
            
            checked = rep(checked, length(items))

            group = ggroup(horizontal = horizontal, container=container, ...)
            
            lst = list()
            n = length(items)
            for(i in 1:n) {
              newItem = gcheckbox(items[i], checked=checked[i], cont=group)
              lst[[ as.character(items[i]) ]] = newItem
            }
  

            theArgs = list(...)
            if(!is.null(theArgs$coerce.with)) {
              coerce.with = theArgs$coerce.with
            } else {
              if(is.numeric(items))
                coerce.with = as.numeric
              else
                coerce.with = as.character
            }
            if(is.character(coerce.with))
              coerce.with = get(coerce.with)

            
            ## make combination widget with all the values
            obj = new("gCheckboxgrouptcltk",block=group, widget=group, toolkit=toolkit, coercewith = coerce.with, e = new.env())
  
            tag(obj, "items") <- items
            tag(obj, "itemlist") <- lst
            
            if(!is.null(handler))
              tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)

            
            return(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...)
            
            lst = tag(obj, "itemlist")
            vals = sapply(lst, svalue)         # logicals

            if(!is.null(index) && index == TRUE) {
              return(which(vals))       # return indices
            } else {
              vals = tag(obj,"items")[vals]
              coerce.with = obj@coercewith
              if(is.null(coerce.with))
                return(vals)
              else
                return(coerce.with(vals))
            }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   lst = tag(obj,"itemlist")
                   n <- length(obj)
                   
                   ## compute values -- logical vector with length n
                   if(!is.null(index) && index) {
                     ## indices
                     values <- rep(FALSE, n)
                     values[value] <- TRUE
                   } else if(!is.logical(value)) {
                     ## characters
                    ind <- match(value, obj[])
                    ind <- ind[!is.na(ind)]
                    values <- rep(FALSE,length=n)
                    values[ind] <- TRUE
                   } else {
                     ## logical vector, we recycle
                     values = rep(value, length.out=n) ## recycle
                   }

                   sapply(1:n, function(i) svalue(lst[[i]]) <- values[i])
                   
                   return(obj)
                 })

## [ and [<- refer to the names -- not the TF values

setMethod("[",
          signature(x="gCheckboxgrouptcltk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxgrouptcltk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            items = tag(x,"items")
            if(missing(i))
              return(items)
            else
              return(items[i])
          })

## assigns names
setReplaceMethod("[",
                 signature(x="gCheckboxgrouptcltk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxgrouptcltk"),
          function(x, toolkit, i, j, ..., value) {
            items = tag(x,"items")
            lst = tag(x,"itemlist")
            n = length(items)

            if(missing(i))
              i = 1:length(items)
  
            if(is.logical(i))
              i = which(i)
            items[i] = value
            
            sapply(1:n, function(i) 
                   lst[[i]][] <- items[i]
                   )
            tag(x,"items") <- items
            tag(x,"itemlist") <- lst
  
             return(x)
          })


setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxgrouptcltk"),
          function(x,toolkit) {
            length(tag(x,"items"))
          })


## inherited enabled isn't workgin                
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
                 function(obj, toolkit, ..., value) {

                   sapply(tag(obj,"itemlist"), function(i)
                          enabled(i,...) <- value)
                   return(obj)
                 })


## handlers should define addHandler class for gradio, gcheckbox, and
## gcheckboxgroup. Each needs to have this pause trick. As it is, if I
## wanted to define a new handler I;d have to copy all but the signal.

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            IDs <- lapply(tag(obj,"itemlist"),function(i) {
              ## pass in obj to actualobj
              addHandlerChanged(i,handler=handler,action=action, actualobj = obj)
            })
            return(IDs)
          })
## clicked is changed
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj, toolkit, handler, action, ...)
          })


setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            tag(obj,"handlerList") <- NULL
            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   removehandler(lst[[i]], ID[[i]])
                 )
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {

            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
                   blockhandler(lst[[i]], ID[[i]])
                   )
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {

            lst <- tag(obj,"itemlist")
            sapply(1:length(lst), function(i)
              unblockhandler(lst[[i]], ID[[i]])
            )
          })

