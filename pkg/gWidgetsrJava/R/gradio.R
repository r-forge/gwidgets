setClass("gRadiorJava",
         representation = representation("gComponentrJava",
           coercewith="NULLorFunction"),
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

## constructor
setMethod(".gradio",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   items, selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL,       
                   ...
                   ) {
            force(toolkit)

            if (length(items)<2)
              stop("Radio button group makes sense only with at least two items.")

            r = .jnew("gWidgetsrJava/gRadio",as.character(items),
              as.integer(selected-1), as.logical(horizontal))


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
            
            obj = new("gRadiorJava",block=r, widget=r,
              toolkit=toolkit, ID=getNewID(),  e = new.env(),
              coercewith = coerce.with)

            
            ## do we add to the container?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE, toolkit=obj@toolkit)
              add(container,  obj, ...)
            }
  
            ## add handler
            if(!is.null(handler))
              addhandlerchanged(obj, handler, action)

            
            invisible(obj)
          })

## methods
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gRadiorJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {

            index = ifelse(is.null(index),FALSE,as.logical(index))

            selInd = .jcall(obj@widget, "I", "getSelectedID")+1
            
            ## return index or value
            if(index) {
              return(selInd)
            } else {
              items = obj[]
              if(!is.null(obj@coercewith))
                return(obj@coercewith(items[selInd]))
              else
                return(items[selInd])
            }
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gRadiorJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   
                   if(!is.null(index) && index==TRUE) {
                     ind = value
                   } else {
                     items = obj[]
                     if(value %in% items) {
                       ind = min(which(value == items))
                     } else {
                       ind = -1
                     }
                   }
                   if(ind >= 0)
                     .jcall(obj@widget, "V", "setSelectedID",
                            as.integer(ind-1))

                   return(obj)
                 })


setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gRadiorJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            ## return(items)
            cbg = getWidget(x)
            n = checkboxLength(x)
            items = sapply(1:n,function(j)
              .jcall(cbg,"S","getLabel",as.integer(j-1))
              )

            if(missing(i))
              items[,...,drop=drop]
            else
              items[i,...,drop=drop]
          })
            
setMethod("[",
          signature(x="gRadiorJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

checkboxLength = function(obj) {
  cbg = getWidget(obj)
  n = .jcall(cbg,"I","getLength")
  return(n)
}

## length
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gRadiorJava"),
          function(x,toolkit) {
            return(length(x[]))
          })


## add in markup here
setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gRadiorJava"),
          function(x, toolkit, i, j, ..., value) {

            bg = getWidget(x)
            n = checkboxLength(x)

            if(missing(i))
              i = 1:n
            if(length(value) != length(i)) {
              cat("value has the wrong length\n")
              return(x)
            }

            sapply(1:length(i), function(j) {
              .jcall(bg,"V","setLabel",
                     as.integer(i[j]-1), as.character(value[j]))
            })

            return(x)
          })

setReplaceMethod("[",
                 signature(x="gRadiorJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

##################################################
## handlers


## This handler isn't right. It reacts toa click on the box containing
## the widget, not the radio buttons

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gRadiorJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
              class = "java/awt/event/ActionListener",...)

            return(ID)
            
          })

## click and changed the same
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gRadiorJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj,toolkit,handler,action,...)
          })

