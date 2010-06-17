## TODO: add in [], []<- methods. Use addItem, (how to get length?)
## getItemCount, removeAllItem

## editable has entry widget that can be edited
setClass("gDroplistrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".gdroplist",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   items, selected = 1, # use 0 for blank
                   editable=FALSE,
                   coerce.with = NULL,
                   handler=NULL, action=NULL,
                   container=NULL,
                   ...               # do.quote = TRUE for quote of answer
                   ) {

            force(toolkit)

            ## items could be a data frame, in which case
            ## row 1 has the data, row 2 the icon name

            if(inherits(items,"data.frame")) {
              items <- items[,1, drop=TRUE]
            }
            
            ## items must be a vector here
            items = as.vector(items)              # undoes factor
            items = unique(items)                 # unique
            
            theArgs = list(...)
            ## keep this, but don't advertise
            if(!is.null(theArgs$do.quote)) {
              coerce.with = function(x) paste("'",x,"'")
            }
            
            
            ## droplist is not happy with datastore class
            ## droplist was not happy with numeric vectors! seems strange
            
            combo =  .jnew("javax/swing/JComboBox")
            if(editable)
              .jcall(combo,"V","setEditable",TRUE)

            obj = new("gDroplistrJava",block=combo,widget=combo,
              toolkit=toolkit,ID=getNewID(),  e = new.env())
            tag(obj,"coerce.with") <- coerce.with
            tag(obj,"editable") <- editable

            ## load items
            obj[] <- items

            
            ## set selectedl
            if(selected > 0) {
              .jcall(combo,"V","setSelectedIndex",
                     as.integer(min(length(items)-1,selected-1)))
            }
            
 ##             ## add drophandler -- switch if drop matches
 ##             adddroptarget(obj, handler = function(h,...) {
 ##               name = id(h$dropdata)
 ##               theValues = obj[]
 ##               if(!is.na(name) && !is.null(name) && name %in% theValues) {
 ##                 svalue(obj) <- name
 ##               }
 ##             })
            ## cat("gdroplist: Add drop handler\n" )

            ## width?
            if(!is.null(theArgs$width)) 
              size(obj) <- c(theArgs$width, -1)

            
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow(visible=TRUE)
              add(container, obj, ...)
            }
            
            if (!is.null(handler)) {
              id <- addhandlerchanged(obj, handler, action)
              tag(obj, "handler.id") <- id
            }
            
            invisible(obj)
          })
          
### methods
## value is for getting/setting the selected value
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gDroplistrJava"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## add in an as.numeric flag, getwidget when editable
            theArgs = list(...)         # deprecated
            coerce.with = tag(obj, "coerce.with")
            editable = tag(obj,"editable")

            val = .jcall(obj@widget,"Ljava/lang/Object;","getSelectedItem")
            selected = .jcall(obj@widget,"I","getSelectedIndex")
            val = .jsimplify(val); selected = .jsimplify(selected)

            if(!is.null(index)) index=as.logical(index)
            
            
            ## selected is the index. It is 0 based
            if(!is.null(editable) && editable == TRUE) {
              ## no check on index -- not applicable
              return(val)
            } else {
              if(!is.null(index) && index==TRUE) {
                return(selected + 1)
              } else {
                if(is.null(coerce.with))
                  return(val)
                else if(is.function(coerce.with))
                  return(coerce.with(val))
                else if(is.character(coerce.with))
                  return(do.call(coerce.with,list(val)))
                else
                  warning("Error: coerce.with is a function or character")
              }
              
            }
          })

## set the displayed value to value
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gDroplistrJava"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   theArgs = list(...)
                   
                   n = length(obj)

                   if(is.null(index))
                     index <- FALSE
                   index <- as.logical(index)



                   ##  if editable do differently
                   ## editable not implented
                   editable = tag(obj,"editable")

                   if(!is.null(editable) && editable) {
                     if(index == TRUE)  {
                       ## set the index
                       .jcall(obj@widget,"V","setSelectedIndex",
                              as.integer(min(n,value-1)))
                     } else {
                       .jcall(obj@widget,"V","setSelectedItem",
                              asjobject(value))
                     }
                   } else {
                     ## not editable
                     if(index) {
                       ## set the index
                       .jcall(obj@widget,"V","setSelectedIndex",
                              as.integer(min(n,value-1)))
                     } else {
                       ## find the value in the list
                       items = obj[]
                       if(any(value == items)) {
                         ind = min(which(value==items))
                         .jcall(obj@widget,"V","setSelectedIndex",
                                as.integer(min(n,ind-1)))
                       } else {
                         ## add to end
                         item = .jnew("java/lang/String",as.character(value))
                         .jcall(obj@widget,"V","addItem",
                                .jcast(item,"java/lang/Object"))
                         .jcall(obj@widget,"V","setSelectedIndex",as.integer(n))
                       }
                     }
                   }
                   return(obj)
                 })

setMethod("length",
          signature(x="gDroplistrJava"),
          function(x) {
            .length(x, x@toolkit)
          })
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gDroplistrJava"),
          function(x, toolkit) {
            x = try(.jsimplify(.jcall(x@widget,"I","getItemCount")),
                silent=TRUE)
            if(inherits(x,"try-error"))
              return(NA)
            else
              return(x)
          })


## the methods [ and [<- refer to the pre-defined values in the drop list.
## [
setMethod("[",
          signature(x="gDroplistrJava"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })
setMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gDroplistrJava"),
          function(x, toolkit, i, j, ..., drop=TRUE) {

            n = length(x)               # no. items
            if(n == 0)
              return(NA)
            
            items = sapply(1:n, function(j) {
              jobj = .jcall(x@widget,"Ljava/lang/Object;","getItemAt",
                as.integer(j-1))
              .jsimplify(jobj)
            })
              
            if(missing(i))
              return(items)
            else
              return(items[i])
          })


## replaces the values in droplist
## values is a vector of values -- not a dataframe
#set.values.gDropList = function(obj, values, ...) {
setReplaceMethod("[",
                 signature(x="gDroplistrJava"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitrJava",x="gDroplistrJava"),
          function(x, toolkit, i, j, ..., value) {


            if(missing(i)) {
              if(length(x) > 0)
                .jcall(x@widget,"V","removeAllItems")
              
              ## add one by one using addItem
              if(!is.data.frame(value))
                value <- data.frame(value, stringsAsFactors=FALSE)
              for(i in as.character(value[,1,drop=TRUE]))
                .jcall(x@widget,"V","addItem",asjobject(i))
            } else {
              items = x[]
              items[i] <- value
              x[] <- items ## recurse
            }

            return(x)
          })

###################################################
  
### handlers
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gDroplistrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ID = addJHandler(obj,handler, action,
              type="addActionListener",
              event = "ActionEvent",
              class = "java/awt/event/ActionListener",...)
            return(ID)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gDroplistrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerchanged(obj,"changed",handler,action)
          })

