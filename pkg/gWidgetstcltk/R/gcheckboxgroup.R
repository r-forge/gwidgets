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

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning(gettext("Container is not correct. No NULL containers possible\n" ))
              return()
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


            tt = getWidget(container)

            cbg_widget <- getRefClass("CheckButtonGroup")$new(parent=tt, items=items,
                                                              selected=checked, horizontal=horizontal)

            obj <- new("gCheckboxgrouptcltk", block=cbg_widget$get_widget(), widget=cbg_widget$get_widget(),
                       toolkit=toolkit, coercewith = coerce.with, e = new.env())
            tag(obj, "cbg_widget") <- cbg_widget

            
            ## checked = rep(checked, length(items))

            ## group = ggroup(horizontal = horizontal, container=container, ...)
            
            ## lst = list()
            ## n = length(items)
            ## for(i in 1:n) {
            ##   newItem <- gcheckbox(items[i], checked=checked[i], cont=group, anchor=c(-1,0))
            ##   lst[[ as.character(items[i]) ]] <- newItem
            ## }
  

            ## theArgs = list(...)
            ## if(!is.null(theArgs$coerce.with)) {
            ##   coerce.with = theArgs$coerce.with
            ## } else {
            ##   if(is.numeric(items))
            ##     coerce.with = as.numeric
            ##   else
            ##     coerce.with = as.character
            ## }
            ## if(is.character(coerce.with))
            ##   coerce.with = get(coerce.with)

            
            ## ## make combination widget with all the values
            ## obj = new("gCheckboxgrouptcltk", block=group, widget=group,
            ##   toolkit=toolkit, coercewith = coerce.with, e = new.env())
  
            ## tag(obj, "items") <- items
            ## tag(obj, "itemlist") <- lst
            
            ## if(!is.null(handler))
            ##   tag(obj, "handler.id") <- addhandlerchanged(obj,handler,action)

            
            ## add to container
            add(container,  obj,...)
  
            ## add handler
            if(!is.null(handler))
              tag(obj, "handler.id") <- addhandlerchanged(obj, handler, action)

            invisible(obj)
          })


### methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            
            cbg_widget <- tag(obj, "cbg_widget")
            index <- getWithDefault(index, FALSE)
            if(index) {
              return(cbg_widget$get_index())
            } else {
              val <- cbg_widget$get_value()
              if(!is.null(obj@coercewith))
                return(obj@coercewith(val))
              else
                return(val)
            }
          
            

            ## theArgs = list(...)
            
            ## lst = tag(obj, "itemlist")
            ## vals = sapply(lst, svalue)         # logicals

            ## if(!is.null(index) && index == TRUE) {
            ##   return(which(vals))       # return indices
            ## } else {
            ##   vals = tag(obj,"items")[vals]
            ##   coerce.with = obj@coercewith
            ##   if(is.null(coerce.with))
            ##     return(vals)
            ##   else
            ##     return(coerce.with(vals))
            ## }
          })

## toggles state to be T or F
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   cbg_widget <- tag(obj, "cbg_widget")
                   index <- getWithDefault(index, FALSE)

                   if(index) {
                     cbg_widget$set_index(value)
                   } else if(is.logical(value)) {
                     n <- length(obj)
                     value <- rep(value, lenght.out=n)
                     cbg_widget$set_index(value)
                   } else {
                     cbg_widget$set_value(value)
                   }
                   return(obj)

                   

                   ## lst = tag(obj,"itemlist")
                   ## n <- length(obj)
                   
                   ## ## compute values -- logical vector with length n
                   ## if(!is.null(index) && index) {
                   ##   ## indices
                   ##   values <- rep(FALSE, n)
                   ##   values[value] <- TRUE
                   ## } else if(!is.logical(value)) {
                   ##   ## characters
                   ##  ind <- match(value, obj[])
                   ##  ind <- ind[!is.na(ind)]
                   ##  values <- rep(FALSE,length=n)
                   ##  values[ind] <- TRUE
                   ## } else {
                   ##   ## logical vector, we recycle
                   ##   values = rep(value, length.out=n) ## recycle
                   ## }

                   ## sapply(1:n, function(i) svalue(lst[[i]]) <- values[i])
                   
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
            cbg_widget <- tag(x, "cbg_widget")
            items <- cbg_widget$get_items()
            if(missing(i))
              items
            else
              items[i]
            ## items = tag(x,"items")
            ## if(missing(i))
            ##   return(items)
            ## else
            ##   return(items[i])
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
            cbg_widget <- tag(x, "cbg_widget")
            if(!missing(i)) {
              items <- cbg_widget$get_items()
              items[i] <- value
              value <- items
            }
            cbg_widget$set_items(value)

            ## items = tag(x,"items")
            ## lst = tag(x,"itemlist")
            ## n = length(items)

            ## if(missing(i))
            ##   i = 1:length(items)
  
            ## if(is.logical(i))
            ##   i = which(i)
            ## items[i] = value
            
            ## sapply(1:n, function(i) 
            ##        lst[[i]][] <- items[i]
            ##        )
            ## tag(x,"items") <- items
            ## tag(x,"itemlist") <- lst
  
             return(x)
          })


setMethod(".length",
          signature(toolkit="guiWidgetsToolkittcltk",x="gCheckboxgrouptcltk"),
          function(x,toolkit) {
            cbg_widget <- tag(x, "cbg_widget")
            cbg_widget$no_items()
#            length(tag(x,"items"))
          })


## inherited enabled isn't workgin                
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
                 function(obj, toolkit, ..., value) {
                   cbg_widget <- tag(obj, "cbg_widget")
                   cbg_widget$set_enabled(value)
                   return(obj)
                   ## sapply(tag(obj,"itemlist"), function(i)
                   ##        enabled(i,...) <- value)
                   ## return(obj)
                 })


## This handler code is common to gradio and gcheckboxgroup. Should abstract out into a superclass.
## IF we do that, we should also use CheckButton bit
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            cbg_widget <- tag(obj, "cbg_widget")
            user.data=list(obj=obj, handler=handler, action=action)
            id <- cbg_widget$add_handler("<ButtonRelease-1>",
                                        handler=function(user.data) {
                                          h <- user.data[c("obj", "action")]
                                          user.data$handler(h)
                                  },
                                        user.data=user.data)
            invisible(id)
            
          })
## clicked is changed
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })


## should have gradio and gcheckboxgroup have same parent class so we can define these together
setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            cbg_widget <- tag(obj, "cbg_widget")
            cbg_widget$remove_handler(ID)
            
            ## sapply(tag(obj,"itemlist"), function(i) {
            ##   .removehandler(i@widget, toolkit, ID, ...)
            ## })
            ## invisible()
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            cbg_widget <- tag(obj, "cbg_widget")
            cbg_widget$block_handler(ID)
            
            ## sapply(tag(obj,"itemlist"), function(i) {
            ##   .blockhandler(i@widget, toolkit, ID, ...)
            ## })
            ## invisible()
          })

##' call to unblock a handler by ID. If ID=NULL, all handlers are unblocked
setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gCheckboxgrouptcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            cbg_widget <- tag(obj, "cbg_widget")
            cbg_widget$unblock_handler(ID)

            ## sapply(tag(obj,"itemlist"), function(i) {
            ##   .unblockhandler(i@widget, toolkit, ID, ...)
            ## })
            ## invisible()
          })
