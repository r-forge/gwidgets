missingMsg = function(x) {
  if(missing(x)) x = "XXX"
  gwCat("This method",x,"needs to be written\n")
}


## toolkit class
## register classes here for toolkits
setClass("guiWidgetsToolkitrJava",
         contains="guiWidgetsToolkit",
         prototype=prototype(new("guiWidgetsToolkit"))
         )




##################################################
## put S3 classes from rJava into S4 classes
## got these from apropos("New") -> try(class(do.call(i,list())))
## oldClasses =
## c(
##   "jobjRef"
## )

require(rJava)
setClass("rJavaObject")
#setOldClass("jobjRef")
#setIs("jobjRef","rJavaObject")
## sapply(oldClasses, function(i) {
##   setOldClass(i)
##   setIs(i,"rJavaObject")
## })

setOldClass("try-error")                # for handling try-errors


## a base class which is virtual


##################################################
## A virtual class to hold either RGTK or these guys

## A virtual class for our newly defined objects
## this one contains the ID for the object.
## this may better be done within the NAMESPACE

n=0;assignInNamespace("n",0,"gWidgetsrJava")
getNewID = function() {                 # get new one, incremented
  n = getFromNamespace("n",ns="gWidgetsrJava")
  assignInNamespace("n",n+1,ns="gWidgetsrJava")
  return(n+1)
}
         

setClass("gWidgetrJava",
         representation(ID="numeric",
                        e = "environment"),
         )


setClassUnion("guiWidgetORgWidgetrJavaORrJavaObject",
              c("guiWidget","gWidgetrJava","jobjRef", "rJavaObject"))

## subclss
setClass("gComponentrJava",
         representation(
                        block="guiWidgetORgWidgetrJavaORrJavaObject",
                        widget="guiWidgetORgWidgetrJavaORrJavaObject",
                        toolkit="guiWidgetsToolkit"
                        ),
         contains="gWidgetrJava",
         )
setClass("gContainerrJava",
         representation(
                        block="guiWidgetORgWidgetrJavaORrJavaObject",
                        widget="guiWidgetORgWidgetrJavaORrJavaObject",
                        toolkit="guiWidgetsToolkit"
                   ),
         contains="gWidgetrJava",
         )





##################################################
### Common methods.    Specific to a class are put into the file for that class

## we have two definitions. For instance, "svalue" and ".svalue". The "svalue" method dispatches on the object to the .svalue method. This allows us to use svalue instead of .svalue when defining the methods/constructors inside this package.


setMethod("svalue",signature(obj="gWidgetrJava"),
          function(obj, index=NULL, drop=NULL, ...) {
            .svalue(obj, obj@toolkit, index=index, drop=drop, ...)
          })



## svalue
## need method for character
setMethod("svalue",signature(obj="character"),
          function(obj, index=NULL, drop=NULL, ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(obj)
                   )
          })
setMethod(".svalue",signature(toolkit = "guiWidgetsToolkitrJava", obj="character"),
          function(obj, toolkit, index=NULL, drop=NULL,  ...)  {
            ifelse(length(obj) == 1,
                   return(getObjectFromString(obj)),
                   return(NA)
                   )
          })

## svalue<- -- objec specific
setReplaceMethod("svalue",signature(obj="gWidgetrJava"),
          function(obj, index=NULL, ...,value) {
            .svalue(obj, obj@toolkit, index=index, ...) <- value
            obj
          })

                   
                 
## [
setMethod("[",
          signature(x="gWidgetrJava"),
          function(x,i,j,...,drop=TRUE) {
            
            return(.leftBracket(x, x@toolkit,i,j,...,drop=TRUE))
          })

## [<-
setReplaceMethod("[",signature(x="gWidgetrJava"),
          function(x,i,j,...,value) {
            if(missing(i) && missing(j))
              .leftBracket(x, x@toolkit,...) <- value
            else if(missing(j))
              .leftBracket(x, x@toolkit,i,...) <- value
            else 
              .leftBracket(x, x@toolkit,i,j,...) <- value
            return(x)
          })

## size ## return size -- not implemented
setMethod("size",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            cat("size not defined, Set window size with size<-()")
            return()
            .size(obj, obj@toolkit,...)
          })

setMethod(".size", 
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) {
            d = obj@widget$getSize()    # a Dimension object
            return(c(.jfield(d,name="width"),
                     .jfield(d,name="height")))
          })

## size<-
setReplaceMethod("size",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .size(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
                 function(obj, toolkit, ..., value) {
                   width = value[1]; height=value[2]

                   
                   widget = obj@widget
                   d = .jnew("java/awt/Dimension", as.integer(width), as.integer(height))
                   .jcall(.jcast(widget,"javax/swing/JComponent"),"V","setMinimumSize",d)
                   ## also set maximum size is possible
                   return(obj)
                 })

## visible
setMethod("visible",signature(obj="gWidgetrJava"),
          function(obj, set=NULL, ...) {
            .visible(obj,obj@toolkit, set=set, ...)
          })

setMethod(".visible",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, set=TRUE, ...) {
            widget = obj@widget

            missingMsg(".visible")
            return()
            
            if(as.logical(set))
              widget$Show()
            else
              widget$Hide()
          })


## visible<-
setReplaceMethod("visible",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .visible(obj, obj@toolkit, ...) <- value
            return(obj)
          })

setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
                 function(obj, toolkit, ..., value) {
                   .visible(obj@widget, toolkit, set=as.logical(value))
                   return(obj)
                 })

## enabled -- not implemeneted, don't   know how to find sensitive. Would need to keep in
##            in the widget using tag or somesuch
setMethod("enabled",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            .enabled(obj, obj@toolkit,...)
          })
setMethod(".enabled",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) {
            widget = getWidget(obj)
            .jcall(.jcast(widget,"java/awt/Component"),"Z","isEnabled")
          })

## enabled<-
setReplaceMethod("enabled",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .enabled(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".enabled",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
                 function(obj, toolkit, ..., value) {
                   widget = getWidget(obj)
                   .jcall(.jcast(widget,"java/awt/Component"),"V",
                          "setEnabled",as.logical(value))
                   return(obj)
                 })


## tooltip<-
setReplaceMethod(".tooltip",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
                 function(obj, toolkit, ..., value) {
                   widget = getWidget(obj)
                   .jcall(.jcast(widget,"java/awt/Component"),"V",
                          "setToolTipText",value)
                   return(obj)
                 })

## defaultWidget
setMethod("defaultWidget",
          signature(obj = "gWidgetrJava"),
          function(obj,...) .defaultWidget(obj,obj@toolkit,...)
          )

setMethod(".defaultWidget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) {
            .defaultWidget(obj, toolkit) <-  TRUE
            })

## defaultWidget<-
setReplaceMethod("defaultWidget",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .defaultWidget(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".defaultWidget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ..., value) {
            ## XXX What to do for default widget?
            ## can do something for buttons, but that's it
            return(obj)
          })
                 

## focus
setMethod("focus",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            .focus(obj, obj@toolkit,...)
          })

setMethod(".focus",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) focus(obj) <- TRUE)

## focus<-
setReplaceMethod("focus",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .focus(obj, obj@toolkit,...) <- value
            return(obj)
          })

setReplaceMethod(".focus",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ..., value) {
            focus(obj@widget, toolkit, ...) <- value
            return(obj)
          })
                 

setReplaceMethod(".focus",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, ..., value) {
            notImplemented("rJavaObject",".focus")
            value = as.logical(value)

            if(value)
              ## fpcis obj

            return(obj)

          })

## font
## weight and style are f*ed up
.font.styles = list(
  families = c("normal","sans","serif","monospace"),
  weights = c("normal","oblique","italic"),
  styles = c("ultra-light","light","normal","bold","ultra-bold","heavy"),
  colors = c("black","blue","red","green","brown","yellow","pink")
)  

## The .font method is not imported from gWidgets, or exported from gWidgetsrJava. Add this bac if you are going to use this method

setMethod("font",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            cat("font() not defined. Set fonts with font<-")
            return()
            .font(obj, obj@toolkit,...)
          })

## font<-
setReplaceMethod("font",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            .font(obj, obj@toolkit,...) <- .fixFontMessUp(value)
            return(obj)
          })
setReplaceMethod(".font",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
                 function(obj, toolkit, ..., value) {
                   ## return obj!!
                   missingMsg(".font<-");return(obj)

                   ##################################################
                   ## This is from RGtk2, need to modify

                   if(!is.list(value))
                     value <- lapply(value, function(i) i)

                   string = ""
                   if(!is.null(value$family) && value$family %in% .font.styles$families)
                     string = Paste(string," ",value$family)
                   if(!is.null(value$weight) && value$weight %in% .font.styles$weights)
                     string = Paste(string," ",value$weight)
                   if(!is.null(value$style) && value$style %in% .font.styles$styles)
                     string = Paste(string," ",value$style)
                   if(!is.null(value$color) && value$color %in% .font.styles$colors)
                     string = Paste(string," ",value$color)
                   if(!is.null(value$size))
                     string = Paste(string," ",as.integer(value$size))
                   

##                   fontDescr = pangoFontDescriptionFromString(string)
##                   getWidget(obj)$ModifyFont(fontDescr)
                   
                   return(obj)
                 })
## tag, tag<-
## This is now from gWidgets by default

## In RGtk2 we used the getData() and setData() methods. In rJava I'd like
## to use java/util/Properties, but that doesn't allow enough flexibility for
## values, so instead we use a list in R to do this. The drawback is that when an object is deleted, we don't clear out the contents -- grows without bound!

## ## create namespace object
## #tags = list()
## #assignInNamespace("tags",list(),"gWidgetsrJava")

## ## clear out tags for this ID. Not exported. Is this used?
## Tagsclear = function(obj) {

##   id = obj@ID
  
##   tags = getFromNamespace("tags",ns="gWidgetsrJava")
##   allKeys = names(tags)

##   inds = grep(paste("^",id,"::",sep=""),allKeys)
##   if(length(inds) == 0)
##     return(NA)

##   ## else
##   tags[[inds]] <- NULL
##   assignInNamespace("tags",tags,ns="gWidgetsrJava")
## }


setMethod("tag",signature(obj="gWidgetrJava"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE
            .tag(obj, obj@toolkit,i, drop=drop,...)
          })
## dispatch in *this* toolkit, not present in obj
setMethod("tag",signature(obj="rJavaObject"),
          function(obj,i,drop=TRUE, ...) {
            if(missing(drop)) drop <- TRUE            
            .tag(obj, guiToolkit("rJava"),i, drop=drop,...)
          })

setMethod(".tag", signature(toolkit="guiWidgetsToolkitrJava",obj="guiWidget"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                        
            .tag(obj@widget,toolkit,  i, drop=drop,  ...)
          })
setMethod(".tag", signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, i, drop=TRUE, ...) {
            if(missing(i)) i = NULL
            if(missing(drop)) drop <- TRUE                                    

            
            if(is.null(i))
              return(as.list(obj@e))
            else
              return(obj@e[[i]])
            

            
##             id = obj@ID

##             ## get all values for this id
##             tags = getFromNamespace("tags",ns="gWidgetsrJava")
##             allKeys = names(tags)

##             inds = grep(paste("^",id,"::",sep=""),allKeys)
##             if(length(inds) == 0)
##               return(NULL)

##             justTheKeys = sapply(allKeys[inds],function(keyWithID) {
##               sub(paste("^",id,"::",sep=""),"",keyWithID)
##             })

##             tagByKey = list()
##             for(key in justTheKeys) 
##               tagByKey[[key]] = tags[[paste(id,key,sep="::")]]
                      
            
            
##             if(is.null(i)) return(tagByKey)

##             if(drop) {
##               if(length(i) == 1)
##                 return(tagByKey[[i]])
##               else
##                 return(sapply(i, function(j) tagByKey[j]))
##             } else {
##               return(sapply(i, function(j) tagByKey[j]))
##             }
          })

## tag <-
setReplaceMethod("tag",signature(obj="gWidgetrJava"),
          function(obj, i, replace=TRUE, ..., value) {
            .tag(obj, obj@toolkit,i,replace, ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("tag",signature(obj="rJavaObject"),
          function(obj,i, replace=TRUE, ..., value) {
            .tag(obj, guiToolkit("rJava"),i, replace, ...) <- value
            return(obj)
          })

## objects can be in many different flavors: guiWIdget, gWidgetrJava, rJavaObject
setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkitrJava",obj="guiWidget"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL
            .tag(obj@widget,toolkit,  i, replace, ...) <- value
            return(obj)
          })

setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, i, replace=TRUE, ..., value) {
            if(missing(i)) i = NULL

            obj@e[[i]] <- value
            return(obj)


##             id = obj@ID
##             key = paste(id,i,sep="::")
            
##             ## if we append we need to work a little harder
##             tags = getFromNamespace("tags",ns="gWidgetsrJava")
  
##             if(replace==FALSE) {
##               value = c(tags[[key]],value)
##             }

##             tags[[key]] <- value
##             assignInNamespace("tags", tags,ns="gWidgetsrJava")

            return(obj)

          })
## setReplaceMethod(".tag", signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
##           function(obj, toolkit, i, replace=TRUE, ..., value) {
##             if(missing(i) || is.null(i)) {
##               warning("Need to specify a key to the 'i' argument of tag<-")
##             } else {
##               theArgs = list(...)
##               replaceIt = as.logical(replace)

##               missingMsg(".tag<-");return()
              
##               allData = obj$GetData(".tagKey")
##               if(is.null(allData)) allData = list()
              
##               if(replaceIt) {
##                 allData[[i]] <- value
##               } else {
##                 allData[[i]] <- c(allData[[i]], value)
##               }
##               obj$SetData(".tagKey", allData)
##             }
##             return(obj)
##           })


##################################################
## id -- define for "ANY" as well
setMethod("id",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            tag(obj,".rJavaID")
          })
setMethod("id",signature(obj="rJavaObject"),
          function(obj, ...) {
            tag(obj, ".rJavaID", ...)
            return(obj)
          })
setMethod("id",signature(obj="ANY"),
          function(obj, ...) {
            if(!is.null(theID<- attr(obj,"id"))) {
              return(theID)
            } else {
              if(is.character(obj)) {
                return(obj[1])
              } else {
                dps = deparse(substitute(obj))
                attr(obj,"id") <- dps
                return(dps)
              }
            }
          })


setMethod(".id", signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) {
            tag(obj,".rJavaID", ...)
          })
setMethod(".id", signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit,  ...) {
            return(tag(obj,".rJavaID"))
          })


## id<-
setReplaceMethod("id",signature(obj="gWidgetrJava"),
          function(obj, ..., value) {
            tag(obj,".rJavaID", ...) <- value
            return(obj)
          })
## dispatch in *this* toolkit, not present in obj
setReplaceMethod("id",signature(obj="rJavaObject"),
          function(obj, ..., value) {
            tag(obj, ".rJavaID", ...) <- value
            return(obj)
          })
setReplaceMethod("id",signature(obj="ANY"),
          function(obj, ..., value) {
            attr(obj,"id") <- value
            return(obj)
          })


## we need a .id to handle dispatch from guiWidgets, otherwise, we use id()
setReplaceMethod(".id", signature(toolkit="guiWidgetsToolkitrJava",
                                  obj="gWidgetrJava"),
          function(obj, toolkit, ..., value) {
            id(obj, ...) <- value
            return(obj)
          })



## add method is biggie
## we have several levels of classes here guiWidget -- gWidgetRGkt -- rJavaObject, when
## we get down to that level we can finally add
setMethod("add",signature(obj="gWidgetrJava"),
          function(obj, value, ...) {
            .add(obj, obj@toolkit,value,...)
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="guiWidget", value="ANY"),
          function(obj, toolkit, value, ...) {
            cat("Can't add without a value\n")
          })
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gWidgetrJava", value="try-error"),
          function(obj, toolkit, value, ...) {
            gmessage(paste("Error:",obj))
          })
## pushdonw
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="guiWidget", value="guiWidgetORgWidgetrJavaORrJavaObject"),
          function(obj, toolkit, value, ...) {
            .add(obj@widget, toolkit, value, ...)
          })

## for gWindow
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",
                    obj="gContainerrJava", value="guiWidget"),
          function(obj, toolkit, value, ...) {
            .add(obj, toolkit, value@widget, ...)
          })

## for gContainer
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava", value="gWidgetrJava"),
          function(obj, toolkit, value, ...) {
            theArgs = list(...)
            expand = if(is.null(theArgs$expand)) FALSE else theArgs$expand

            tag(value, "parentContainer") <- obj
            
            cont = getWidget(obj)
            widget = getBlock(value)

            if(expand) {
              maxValue = 1600           # MAX_VALUE isn't working
              d = .jnew("java/awt/Dimension",as.integer(maxValue),as.integer(maxValue))
              .jcall(.jcast(widget, "java/awt/Component"),"V",
                     "setMaximumSize",d)
            }

            anchor = theArgs$anchor

            ## initial placement high and outside! This is GTK2 like
            if(is.null(anchor)) anchor = c(-1,1)
            
            if(!is.null(anchor)) {
              tmp = .jcast(widget,"java/awt/Component")
              if(anchor[1] == 1)
                .jcall(widget,"V","setAlignmentX",.jfloat(.jfield(tmp,name="RIGHT_ALIGNMENT")))
              else if(anchor[1] == 0)
                .jcall(widget,"V","setAlignmentX",.jfloat(.jfield(tmp,name="CENTER_ALIGNMENT")))
              else 
                .jcall(widget,"V","setAlignmentX",.jfloat(.jfield(tmp,name="LEFT_ALIGNMENT")))

              if(anchor[2] == 1)
                .jcall(widget,"V","setAlignmentY",.jfloat(.jfield(tmp,name="TOP_ALIGNMENT")))
              else if(anchor[2] == 0)
                .jcall(widget,"V","setAlignmentY",.jfloat(.jfield(tmp,name="CENTER_ALIGNMENT")))
              else 
                .jcall(widget,"V","setAlignmentY",.jfloat(.jfield(tmp,name="BOTTOM_ALIGNMENT")))
              }

            .jcall(cont,"Ljava/awt/Component;", "add",
                   .jcast(widget, "java/awt/Component"))



            ## update
            .jcall(cont, "V", "invalidate")
            .jcall(cont, "V", "validate")

            ## pack
            top <- getTopLevel(obj)
            if(!is.null(top)) {
              .jcall(getBlock(top), "V", "pack")
            }
          })


## setMethod(".add",
##           signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava", value="gWidgetrJava"),
##           function(obj, toolkit, value, ...) {
##             .add(obj, toolkit, value@block, ...)
##           })




## addSPring, addSpace
setMethod("addSpring",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            .addSpring(obj, obj@toolkit,...)
          })

setMethod(".addSpring",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava"),
          function(obj, toolkit, ...) {

            ## is glue the right thing here?
            tmp = obj@widget
            if(obj@horizontal)
              glue = tmp$createHorizontalGlue()
            else
              glue = tmp$createVerticalGlue()
            .jcall(tmp,"Ljava/awt/Component;","add",
                   .jcast(glue,"java/awt/Component"))
          })

setMethod("addSpace",signature(obj="gWidgetrJava"),
          function(obj, value, ...) {
            .addSpace(obj,obj@toolkit,value,...)
          })

setMethod(".addSpace",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava"),
          function(obj, toolkit, value, ...) {
            theArgs = list(...)
            horizontal = ifelse(is.null(theArgs$horizontal),
              TRUE,
              as.logical(theArgs$horizontal))

            ## use createRigidArea with Dimension
            if(horizontal)
              d = .jnew("java/awt/Dimension",as.integer(value),as.integer(0))
            else
              d = .jnew("java/awt/Dimension",as.integer(0),as.integer(value))

            cont = getWidget(obj)
            tmp = cont$createRigidArea(d)
##             tmp = .jcall(.jcast(getWidget(obj),"javax/swing/Box"),
##                    "Ljava/awt/Component;",
##                    "createRigidArea",d)
            ## now add to box
            .jcall(cont,
                   "Ljava/awt/Component;",
                   "add",
                   .jcast(tmp,"java/awt/Component"))
          })

## delete -- get down to two rJavaObjects
setMethod("delete",signature(obj="gWidgetrJava"),
          function(obj, widget, ...) {
            .delete(obj, obj@toolkit,widget,...)
          })

## push down to rJava vs rJava. Can be 9 possiblities!
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava",widget="guiWidget"),
          function(obj, toolkit, widget, ...) {
            .delete(obj, toolkit, widget@widget, ...)
          })
setMethod(".delete",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gContainerrJava",widget="gWidgetrJava"),
          function(obj, toolkit, widget, ...) {
            ## call remove on container
            cont = getWidget(obj)
            widget = getWidget(widget)
            .jcall(cont, "V", "remove",  .jcast(widget, "java/awt/Component"))
            
          })

## dispose -- delete the parent window, or something else
setMethod("dispose",signature(obj="gWidgetrJava"),
          function(obj, ...) {
            .dispose(obj, obj@toolkit,...)
          })

setMethod(".dispose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ...) {


            missingMsg(".dispose");return()
            
          })




## update
setMethod("update",signature(object="gWidgetrJava"),
          function(object, ...) {
            .update(object, object@toolkit, ...)
          })

setMethod(".update",
          signature(toolkit="guiWidgetsToolkitrJava",object="gComponentrJava"),
          function(object, toolkit, ...) {

            missingMsg(".update");return()
            
            object@widget$QueueDraw()
          })

##
##
##################################################


##################################################
## handlers
##
## basic handler for adding with a signal.
setGeneric("addhandler", function(obj, signal, handler, action=NULL, ...)
           standardGeneric("addhandler"))
setMethod("addhandler",signature(obj="guiWidget"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj@widget, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="gWidgetrJava"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj, obj@toolkit, signal, handler, action, ...)
          })
setMethod("addhandler",signature(obj="rJavaObject"),
          function(obj, signal, handler, action=NULL, ...) {
            .addHandler(obj, guiToolkit("rJava"), signal, handler, action, ...)
          })

## method for dispatch
setGeneric(".addHandler",
           function(obj, toolkit,
                  signal, handler, action=NULL, ...)
           standardGeneric(".addHandler"))


setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="guiWidget"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            .addHandler(obj@widget, toolkit, signal, handler, action, ...)
          })

setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {

##             missingMsg(".addHandler");return()

            
##             ## need to return TRUE
##             modifyHandler = function(...) {
##               handler(...)
##               return(TRUE)
##             }



            
##             callbackID <- try(connectSignal(getWidget(obj), ### issue: getWidget(obj),
##                                             signal=signal,
##                                             f=modifyHandler,
##                                             data=list(obj=obj, action=action,...),
##                                             user.data.first = TRUE,
##                                             after = FALSE), silent=FALSE)
##             if(inherits(callbackID,"try-error")) {
##               cat("Couldn't add signal: ",signal," for object of class:")
##               cat(class(obj))
##               return(NA)
##             } else {
##               ## now put handler into object
##               handler.ID = tag(obj, "handler.id")
##               if(is.null(handler.ID))
##                 handler.ID =list()
##               handler.ID[[length(handler.ID)+1]] = callbackID
##               tag(obj, "handler.id") <- handler.ID
              
##               ##
##               ##            addhandlerdestroy(obj, handler=function(h,...)
##               ##                              removehandler(h$obj,h$action),
##               ##                              action=ID)
##               ## return ID
##               invisible(callbackID)
##            }
          })

setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {
##             callbackID <- try(connectSignal(obj,
##                                             signal=signal,
##                                             f=handler,
##                                             data=list(obj=obj, action=action, ...),
##                                             user.data.first = TRUE,
##                                             after = FALSE),
##                               silent=TRUE)
##             ## can't' stuff in handler IDS
##             if(inherits(callbackID,"try-error")) {
##               cat("Couldn't connect signal:",signal,"for")
##               print(obj)
##               return(NA)
##             } else {
##               invisible(callbackID)
##             }
          })

## removew handler
## removehandler
setMethod("removehandler", signature("gWidgetrJava"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, obj@toolkit, ID, ...)
          })
setMethod("removehandler", signature("rJavaObject"),
          function(obj, ID=NULL, ...) {
            .removehandler(obj, guiToolkit("rJava"), ID, ...)
          })

setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ID=NULL, ...) {

            ## ID here has two components
            type = ID[1]
            handlerID=as.character(ID[2])
            ID = as.character(obj@ID)

            ## remove from list
            allHandlers = getFromNamespace("allHandlers",ns="gWidgetsrJava")

            ## is this a idleHandler
            if(type == "addIdleListener") {
              t = allHandlers[[ID]][[type]][[handlerID]]$timer
              t = .jcall(t,"V","stopTimer")
            }
            allHandlers[[ID]][[type]][[handlerID]]<-NULL
              ## now store the hash
            assignInNamespace("allHandlers",allHandlers,ns="gWidgetsrJava")
          })


## blockhandler
setMethod("blockhandler", signature("gWidgetrJava"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj, obj@toolkit, ID, ...)
          })
setMethod("blockhandler", signature("rJavaObject"),
          function(obj, ID=NULL, ...) {
            .blockhandler(obj, guiToolkit("rJava"), ID, ...)
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ID=NULL, ...) {
            .blockhandler(getWidget(obj),toolkit,ID,...)
          })

setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, ID=NULL, ...) {
            cat("define block handler\n")
          })

## unblock handler
setMethod("unblockhandler", signature("gWidgetrJava"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(obj, obj@toolkit, ID, ...)
          })
setMethod("unblockhandler", signature("rJavaObject"),
          function(obj, ID=NULL, ...) {
            .unblockhandler(obj, guiToolkit("rJava"), ID, ...)
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, ID=NULL, ...) {
            .blockhandler(getWidget(obj),toolkit,ID,...)
          })

setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, ID=NULL, ...) {
            cat("define unblock handler\n")
          })




## addhandlerchanged
setMethod("addhandlerchanged",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, obj@toolkit, handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerchanged(obj, guiToolkit("rJava"), handler, action, ...)
          })
setMethod("addhandlerchanged",signature(obj="ANY"),
          function(obj, handler=NULL, action=NULL, ...) {
            cat("No method addhandlerchanged for object of class",class(obj),"\n")
          })

setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="changed",
                        handler=handler, action=action, ...)
          })


## expose: expose-event or realize
setMethod("addhandlerexpose",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerexpose",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerexpose(obj, guiToolkit("rJava"), handler, action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="expose-event",
                        handler=handler, action=action, ...)
          })

setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gComponentrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj,toolkit, signal="realize",
                        handler=handler, action=action, ...)
          })

## unrealize: unrealize
setMethod("addhandlerunrealize",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerunrealize",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerunrealize(obj, guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerunrealize",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="unrealize",
                        handler=handler, action=action, ...)
          })

## destroy: destroy
setMethod("addhandlerdestroy",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdestroy",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdestroy(obj, guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerdestroy",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="destroy",
                        handler=handler, action=action, ...)
          })

## keystroke: changed
setMethod("addhandlerkeystroke",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerkeystroke",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerkeystroke(obj, guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerkeystroke",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="changed",
                        handler=handler, action=action, ...)
          })

## clicked: clicked
setMethod("addhandlerclicked",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerclicked",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerclicked(obj, guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            .addHandler(obj, toolkit, signal="clicked",
                        handler=handler, action=action, ...)
          })

## doubleclick: no default
setMethod("addhandlerdoubleclick",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerdoubleclick",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerdoubleclick(obj,guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerdoubleclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            cat("No default handler for double click")
          })

## rightclick: button-press-event -- handle separately
setMethod("addhandlerrightclick",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerrightclick",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerrightclick(obj,guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerrightclick",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            ## implement for rJava
##             connectSignal(obj@widget,
##                           signal = "button-press-event",
##                           f = function(h, eventButton,...) {
##                             if(eventButton$GetButton() == 3) {
##                               h$handler(h,...)
##                             }
##                             return(FALSE)         # continue propagation
##                           },
##                           data = list(obj=obj, action=action, handler=handler),
##                           user.data.first = TRUE,
##                           after = FALSE
##                         )
          })

## mouse motion -- like mouseover
setMethod("addhandlermousemotion",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlermousemotion(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlermousemotion",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlermousemotion(obj,guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlermousemotion",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            cat("No default handler for mouse motion")
          })


## blur -- leave widget
setMethod("addhandlerblur",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerblur(obj,obj@toolkit,handler, action, ...)
          })
setMethod("addhandlerblur",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .addhandlerblur(obj,guiToolkit("rJava"),handler, action, ...)
          })

setMethod(".addhandlerblur",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler, action=NULL, ...) {
            cat("No default handler for blur")
          })


## idle
setMethod("addhandleridle",signature(obj="gWidgetrJava"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, obj@toolkit,
                            handler=handler, action=action, interval=interval, ...)
          })
setMethod("addhandleridle",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, interval=1000, ...) {
            .addhandleridle(obj, guiToolkit("rJava"),
                            handler=handler, action=action, interval=interval, ...)
          })

setMethod(".addhandleridle",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit,
                   handler=NULL, action=NULL, interval=1000, ...) {

            ## call gIdle. Using handler set up in aaaHandler
            ## for now, only one such timer
            handlerID = as.character(1)
            
            ID = as.character(obj@ID)
            
            allHandlers = getFromNamespace("allHandlers",ns="gWidgetsrJava")
            if(!is.list(allHandlers))
            if(is.null(allHandlers[[ID]]))
              allHandlers[[ID]] <- list()
            if(is.null(allHandlers[[ID]][["addIdleListener"]]))
              allHandlers[[ID]][["addIdleListener"]] <- list()
            allHandlers[[ID]][["addIdleListener"]][[handlerID]] = list(
                             handler=handler,
                             obj = obj,
                             action=action,
                             envir=parent.frame()
                             )

            ## NOW call timer
            t = .jnew("gWidgetsrJava/gIdle",as.integer(interval),as.integer(ID))
            ## shove into handlers to be able to stop
            allHandlers[[ID]][["addIdleListener"]][[handlerID]]$timer=t
            ## store list
            assignInNamespace("allHandlers",allHandlers,ns="gWidgetsrJava")

            return(c("addIdleListener",handlerID))
          })


## addpopumenu
setMethod("addpopupmenu",signature(obj="gWidgetrJava"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, obj@toolkit,menulist, action, ...)
          })
setMethod("addpopupmenu",signature(obj="rJavaObject"),
          function(obj, menulist, action=NULL, ...) {
            .addpopupmenu(obj, guiToolkit("rJava"), menulist, action, ...)
          })


## ## this does not get exported
addPopupMenuWithSignal = function(obj, toolkit,  menulist, action=NULL, signal="button-press-event", ...) {
  cat("addPopupMenuWithSignal not implemented in gWidgetrJava\n")
  return()
}
##   theArgs = list(...)                      

##   missingMsg("addPopupMenuWithSignal");return()
  
##   f = function(h, ...) {
##     mb = gmenu(h$action, popup = TRUE)
##     event = gdkEventNew(GdkEventType["button-press"])
##     mb = tag(mb,"mb")                   # the real menu bar
##     gtkMenuPopupHack(mb, button = event$GetButton(),
##                      activate.time=event$GetTime()
##                      )
##   }
##   ## .addhandler not exported
##   callbackID = .addHandler(obj,toolkit, signal = signal,handler=f, action=menulist)
##   invisible(callbackID)
## }

add3rdMousePopupMenuWithSignal = function(obj, toolkit,  menulist, action=NULL, signal="button-press-event", ...) {
  cat("3rd mouse popup not implemented in gWidgetsrJava\n")
  return()
}
##   f = function(h, widget, event,...) {
##     if(event$GetButton() != 3) {
##       return(FALSE)                     # propogate signal
##     } else {
##       mb = gmenu(h$action$menulist, popup = TRUE, action=h$action$passedaction)
##       mb = tag(mb,"mb")                 # actual widget
##       gtkMenuPopupHack(mb,button = event$GetButton(),
##                        activate.time=event$GetTime()
##                        )
##     }
##   }
##   callbackID = .addHandler(obj,toolkit, signal = "button-press-event",handler=f, action=list(menulist=menulist, passedaction=action))
##   invisible(callbackID)
## }


  
### need to deal with action
setMethod(".addpopupmenu",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            addPopupMenuWithSignal(obj, toolkit, menulist, ...)
})


## add3rdmousepopupmenu
setMethod("add3rdmousepopupmenu",signature(obj="gWidgetrJava"),
          function(obj, menulist, action=NULL, ...) {
            .add3rdmousepopupmenu(obj, obj@toolkit,menulist, action, ...)
          })

setMethod("add3rdmousepopupmenu",signature(obj="rJavaObject"),
          function(obj, menulist, action=NULL,...) {
            .add3rdmousepopupmenu(obj, guiToolkit("rJava"),menulist, action,...)
          })

setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit, menulist,action=NULL, ...) {
            add3rdMousePopupMenuWithSignal(obj, toolkit,
                                           menulist, action, ...)
          })
setMethod(".add3rdmousepopupmenu",
          signature(toolkit="guiWidgetsToolkitrJava",obj="rJavaObject"),
          function(obj, toolkit, menulist, action=NULL, ...) {
            add3rdMousePopupMenuWithSignal(obj, toolkit,
                                           menulist, action, ...)
          })


## "dotmethods" defined in dnd.R
## adddropsource
setMethod("adddropsource",signature(obj="gWidgetrJava"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })
setMethod("adddropsource",signature(obj="rJavaObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddropsource(obj, guiToolkit("rJava"),targetType=targetType,
                           handler=handler, action=action, ...)
          })

## adddropmotion
setMethod("adddropmotion",signature(obj="gWidgetrJava"),
          function(obj,  handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, obj@toolkit,
                           handler=handler, action=action, ...)
          })
setMethod("adddropmotion",signature(obj="rJavaObject"),
          function(obj, handler=NULL, action=NULL, ...) {
            .adddropmotion(obj, guiToolkit("rJava"),
                           handler=handler, action=action, ...)
          })

## adddroptarget
setMethod("adddroptarget",signature(obj="gWidgetrJava"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, obj@toolkit,targetType=targetType,
                           handler=handler, action=action, ...)
          })

setMethod("adddroptarget",signature(obj="rJavaObject"),
          function(obj, targetType="text", handler=NULL, action=NULL, ...) {
            .adddroptarget(obj, guiToolkit("rJava"),targetType=targetType,
                           handler=handler, action=action, ...)
          })


## R Methods
setMethod("dim", "gWidgetrJava", function(x) .dim(x,x@toolkit))
setMethod(".dim",
          signature(toolkit="guiWidgetsToolkitrJava",x="gWidgetrJava"),
          function(x,toolkit) {
            cat("Define dim for x of class:")
            print(class(x))
            return(NULL)
})
setMethod("length", "gWidgetrJava", function(x) .length(x,x@toolkit))
setMethod(".length",
          signature(toolkit="guiWidgetsToolkitrJava",x="gWidgetrJava"),
          function(x,toolkit) {
            cat("Define length for x of class:")
            print(class(x))
            return(NULL)            
})
          
setMethod("dimnames", "gWidgetrJava", function(x) .dimnames(x,x@toolkit))
setReplaceMethod("dimnames",
                 signature(x="gWidgetrJava"),
                 function(x,value) {
                   .dimnames(x,x@toolkit) <- value
                   return(x)
                 })
## as of 2.5.0 this became primiive
if(as.numeric(R.Version()$major) <= 2 &
   as.numeric(R.Version()$minor) <= 4.1) {
  setGeneric("names")
  setGeneric("names<-")
}

setMethod("names", "gWidgetrJava", function(x) .names(x,x@toolkit))
setReplaceMethod("names",
                 signature(x="gWidgetrJava"),
                 function(x,value) {
                   .names(x,x@toolkit) <- value
                   return(x)
                 })
