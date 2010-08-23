## When you redo this -- to get as.gWIdgetsRGtk2 -- there is
## a need to check that the toggle signal gets called when actually
## pressed, and not deselected.


setClass("gRadioRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

## constructor
setMethod(".gradio",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   items, selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,       
                   ...
                   ) {

            force(toolkit)

            if(horizontal)
              g <- gtkHBox()
            else
              g <- gtkVBox()

            radiogp <- gtkRadioButton(group=NULL, label=items[1]) # initial

            obj <- as.gWidgetsRGtk2(radiogp, block=g)

            if(is.data.frame(items))
              items <- items[,1, drop=TRUE] # first column

            obj[] <- items
            svalue(obj,index=TRUE) <- selected

            tag(obj, ".handlers") <- list() # list of handlers keyed by ID
            
            ## do we add to the container?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=obj@toolkit)
              add(container,  obj,...)
            }
  
            ## add handler
            if(!is.null(handler))
              addHandlerChanged(obj,  handler, action)

            
            invisible(obj)
          })

##' coercion method from a gtkRadioButton widget. Pass in container via bloc
as.gWidgetsRGtk2.GtkRadioButton <- function(widget,...) {
  theArgs <- list(...)
  if(!is.null(theArgs$block))
    block <- theArgs$block
  else
    block <- gtkHBox()                  # or vbox!

  obj <- new("gRadioRGtk",block=block, widget=widget,
    toolkit=guiToolkit("RGtk2"))
  return(obj)
}

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            index = ifelse(is.null(index),FALSE,as.logical(index))

            radiogp <- getWidget(obj)
            btns <- rev(radiogp$GetGroup())
            ind <- sapply(btns, function(i) i$GetActive())

            if(index)
              return(which(ind))
            else
              return(obj[ind])
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
                 function(obj, toolkit, index=NULL, ..., value) {

                   if(is.data.frame(value))
                     value <- value[,1, drop=TRUE]
                   
                   radiogp <- getWidget(obj)
                   btns <- rev(radiogp$GetGroup())
                   items <- obj[]
                   
                   if(!is.null(index) && index==TRUE) {
                     if(value %in% 1:length(obj))
                       btns[[as.numeric(value)]]$SetActive(TRUE)
                     else
                       cat(sprintf("index outside of range\n"))
                   } else {
                     if(value %in% items) {
                       whichIndex = min(which(value == items))
                       btns[[whichIndex]]$SetActive(TRUE)
                     } else {
                       cat(sprintf("Value %s is not among the items\n",value))
                     }
                   }
                   
                   
                   return(obj)
          })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            radiogp <- getWidget(x)
            btns <- rev(radiogp$GetGroup())
            btns <- btns[1:tag(x,".n")]
            items <- sapply(btns, function(i) i$GetLabel())
            
            if(missing(i))
              items
            else
              items[i]
          })
            
setMethod("[",
          signature(x="gRadioRGtk"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x, toolkit, i, j, ..., value) {

            radiogp <- getWidget(x)
            gp <- getBlock(x)
            btns <- rev(radiogp$GetGroup())
            n = length(x)
            
            ## set items
            if(missing(i)) {

              ## The radio group doesn't like to reduce the size. We trick it by
              ## keeping track of a variable length in ".n" tag
              n <- length(value)
              if(n < 2) {
                cat(sprintf("Length of items must be 2 or more\n"))
                return(x)
              }

              ## we store the length of items in the .n value.
              ## When shortening a lenght by setting the group, the GTK
              ## widget does not truncate. We use this to do so. (leaving some
              ## possible orphans in the radioButtonGroup object.)
              tag(x, ".n") <- n

              ## clear old
              sapply(gp$getChildren(), function(i) gp$remove(i))
              ## make new
              radiogp1 <- gtkRadioButton(group=NULL, label=value[1])
              sapply(value[-1], function(i) {
                radiogp1$newWithLabelFromWidget(i)
              })
              ## replace -- doesn't clear, just replaces first n (even if more than n)
              radiogp$setGroup(radiogp1$getGroup())
              
              ## now add to container
              btns <- rev(radiogp$getGroup())[1:n] # no more than n of them
              sapply(btns, function(i) gp$PackStart(i))

              ## need to add in the handlers
              ## Always call to see if a handler exists
              sapply(btns, function(i) {
                gSignalConnect(i, "toggled", f=function(obj, w, ...) {
                  if(w$getActive()) {
                    ## call handlers from h
                    handlers <- tag(obj, ".handlers")
                    if(length(handlers)) {
                      ## handler is list with blocked, handler, action component
                      sapply(handlers, function(handler) {
                        if(!handler$blocked)
                          handler$handler(list(obj=obj, action=handler$action), ...)
                      })
                    }
                  }
                },
                               data=x,
                               user.data.first=TRUE,
                               after=FALSE
                               )
              })
            } else {
              ## update just the i values
              i <- i[i <= n]
              for(j in 1:length(i)) 
                btns[[j]]$SetLabel(value[j])
            }
            
            ## all done
            return(x)
          })

setReplaceMethod("[",
                 signature(x="gRadioRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x,toolkit) {
            tag(x, ".n")
#            radiogp <- getWidget(x)
#            btns <- rev(radiogp$GetGroup())
#            length(btns)
          })


## enabled must go on each button
## enabled <-
setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
                 function(obj, toolkit, ..., value) {
                   radiogp <- getWidget(obj)
                   btns <- rev(radiogp$GetGroup())
                   sapply(btns, function(i) {
                     i$SetSensitive(as.logical(value))
                     })
                   return(obj)
                 })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
                 function(obj, toolkit, ..., value) {
                   radiogp <- getWidget(obj)
                   btns <- rev(radiogp$GetGroup())
                   sapply(btns, function(i) {
                     if(value)
                       i$show()
                     else
                       i$hide()
#                     i$SetSensitive(as.logical(value))
                   })
                   return(obj)
                 })

##################################################
## handlers

## need to deal with changing buttons via [<-
## added a handlers cache that we can manipulate
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {

            handlers <- tag(obj, ".handlers")
            if(length(handlers))
              nhandlers <- max(as.numeric(names(handlers)))
            else
              nhandlers <- 0

            newhandler <- list(blocked=FALSE,
                               handler=handler,
                               action=action)
            ID <- as.character(nhandlers + 1)
            handlers[[ID]] <- newhandler
            tag(obj, ".handlers") <- handlers

            invisible(ID)
            
 
          })
 



setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj,toolkit,handler,action,...)
          })



setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, ID=NULL, ...) {
            handlers <- tag(obj, ".handlers")
            
            if(is.null(ID)) {
              handlers <- list()        # remove all
            } else {
              sapply(ID, function(id) {
                handlers[[id]] <<- NULL
              })
            }
            tag(obj, ".handlers") <- handlers          
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, ID=NULL, ...) {
            handlers <- tag(obj, ".handlers")
            if(is.null(ID)) {
              ID <- names(handlers)
            }
            sapply(ID, function(id) {
              handlers[[id]]$blocked <<- TRUE
            })
            tag(obj, ".handlers") <- handlers          
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, ID=NULL, ...) {
            handlers <- tag(obj, ".handlers")
            if(is.null(ID)) {
              ID <- names(handlers)
            }
            sapply(ID, function(id) {
              handlers[[id]]$blocked <<- FALSE
            })
            tag(obj, ".handlers") <- handlers          
          })

## ## There is an issue here. When we set values via [<- the handlers are gone!
## setMethod(".addhandlerclicked",
##           signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
##           function(obj, toolkit, handler, action=NULL, ...) {

##             radiogp <- getWidget(obj)
##             btns <- rev(radiogp$GetGroup())
            
##             IDs = sapply(btns, function(x) {
##               gtktry(connectSignal(x,
##                                    signal="toggled",
##                                    f=function(h,w,...) {
##                                      ## only call handler for change to active
##                                      ## not just toggle
##                                      if(w$GetActive())
##                                        handler(h,w,...)
##                                    },
##                                    data=list(obj=obj, action=action,...),
##                                    user.data.first = TRUE,
##                                    after = FALSE), silent=FALSE)
##             })
            
##             handler.ID = tag(obj, "handler.id")
##             if(is.null(handler.ID))
##               handler.ID =list()
##             for(i in 1:length(IDs))
##               handler.ID[[length(handler.ID)+1]] = IDs[[i]]
##             tag(obj, "handler.id", replace=FALSE) <- handler.ID
            
##             invisible(IDs)
##           })
 

