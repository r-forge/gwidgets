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

            if(is.data.frame(items))
              items <- items[,1, drop=TRUE] # first column
            
            if (length(items)<2)
              stop("Radio button group makes sense only with at least two items.")
            
            if(horizontal)
              g <- gtkHBox()
            else
              g <- gtkVBox()

            radiogp <- gtkRadioButton(group=NULL, label=items[1]) # initial
            sapply(2:length(items),function(i)
                   radiogp$NewWithLabelFromWidget(items[i]))

            ## when using GetGroup, this gives reverse order
            ## efficiency of gtk list
            

            ## layout
            sapply(rev(radiogp$GetGroup()),         # reverse list
                   function(i) g$PackStart(i))

            obj <- as.gWidgetsRGtk2(radiogp, block=g)

            svalue(obj,index=TRUE) <- selected
            
            
            ## do we add to the container?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=obj@toolkit)
              add(container,  obj,...)
            }
  
            ## add handler
            if(!is.null(handler))
              addhandlerchanged(obj, handler, action)

            
            invisible(obj)
          })

as.gWidgetsRGtk2.GtkRadioButton <- function(widget,...) {
  theArgs <- list(...)
  if(!is.null(theArgs$block))
    block <- theArgs$block
  else
    block <- widget

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
                       cat(sprintf("index outside of range"))
                   } else {
                     if(value %in% items) {
                       whichIndex = min(which(value == items))
                       btns[[whichIndex]]$SetActive(TRUE)
                     } else {
                       cat(sprintf("Value %s is not among the items",value))
                     }
                   }
                   
                   
                   return(obj)
          })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gRadioRGtk"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            radiogp <- getWidget(x)
            btns <- rev(radiogp$GetGroup())
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
            items <- x[]
            n = length(x)
            
            ## set items
            if(missing(i)) {
              ## can't reduce size, just lengthen
              if(length(value) < length(x)) {
                cat(sprintf("Can't reduce length of radio button group"))
                return(x)
              }

              if(n < length(value)) {
                ## add some new buttons; also pack in
                for(i in 1:(length(value)-n)) {
                  b <- radiogp$NewWithLabelFromWidget("")
                  if(inherits(gp,"GtkBox"))
                    gp$PackStart(b)
                  else
                    cat(sprintf("can't pack in object of class %s",class(gp)))
                }
              }
              ## replace all the values
              btns <- rev(radiogp$GetGroup()) # maybe grew in length
              sapply(1:length(btns), function(i)
                     btns[[i]]$SetLabel(value[i]))
            } else {
              ## update just the i values
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
            radiogp <- getWidget(x)
            btns <- rev(radiogp$GetGroup())
            length(btns)
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
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj,toolkit,handler,action,...)
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gRadioRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {

            radiogp <- getWidget(obj)
            btns <- rev(radiogp$GetGroup())
            
            IDs = sapply(btns, function(x) {
              gtktry(connectSignal(x,
                                   signal="toggled",
                                   f=function(h,w,...) {
                                     ## only call handler for change to active
                                     ## not just toggle
                                     if(w$GetActive())
                                       handler(h,w,...)
                                   },
                                   data=list(obj=obj, action=action,...),
                                   user.data.first = TRUE,
                                   after = FALSE), silent=FALSE)
            })
            
            handler.ID = tag(obj, "handler.id")
            if(is.null(handler.ID))
              handler.ID =list()
            for(i in 1:length(IDs))
              handler.ID[[length(handler.ID)+1]] = IDs[[i]]
            tag(obj, "handler.id", replace=FALSE) <- handler.ID
            
            invisible(IDs)
          })
 

