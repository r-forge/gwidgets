##' @include toolkitClasses.R
##' 

##' Basic class for gWidgets objects
setClass("guiWidget",
         representation(
                        toolkit="guiWidgetsToolkit",
                        widget="ANY"  # could be RGtkObject, TclObject,....
                        ),
         prototype(
                   toolkit =guiToolkit(),
                   widget=NULL
                   )
         ) 
           
##' Subclass of gWidgets with NULL possibility
setClassUnion("guiWidgetOrNULL",
              c("NULL","guiWidget"))

##' for ANY toolkit
setClass("gWidgetANY",
         representation(
                        toolkit="guiWidgetsToolkit",
                        widget="ANY",  # could be RGtkObject, TclObject,....
                        block="ANY",
                        ID = "numeric"
                        ),
         prototype(
                   toolkit =guiToolkit(),
                   widget=NULL,
                   block = NULL,
                   ID = getNewID()
                   )
         ) 
           


##################################################
## methods

################ show ##################################
##' show method for guiWidget objects
setMethod("show",signature(object="guiWidget"),
          function(object) {
            cat("guiWidget of type:",
                class(object@widget),
                "for toolkit:",
                class(object@toolkit),
                "\n")
          })

################## svalue ################################
##' define generic for selected value, svalue
setGeneric("svalue",function(obj, index=NULL, drop=NULL,...) standardGeneric("svalue"))

##' base svalue method
setMethod("svalue",signature(obj="guiWidget"),
          function(obj, index=NULL, drop=NULL, ... ) {
            toolkit = obj@toolkit
            .svalue(obj@widget, toolkit, ...,index=index, drop=drop)
          })

##' svalue method for numeric variables
setMethod("svalue",signature(obj="numeric"),
          function(obj, index=NULL, drop=NULL, ... ) {
            return(obj)
          })

##' svalue method for character data
setMethod("svalue",signature(obj="character"),
          function(obj, index=NULL, drop=NULL, ... ) {
            if(length(obj) == 1)
              return(get(obj, envir=.GlobalEnv))
            else
              return(obj)
          })

##' package generic has toolkit, object to dispatch on
##' @alias svalue
setGeneric(".svalue",function(obj, toolkit, index=NULL, drop=NULL,  ...)
           standardGeneric(".svalue"))


################# svalue<- #################################
##' svalue<- generic
setGeneric("svalue<-",function(obj, index=NULL, ...,value) standardGeneric("svalue<-"))

##' svalue method for any widget
setReplaceMethod("svalue",signature(obj="guiWidget"),
          function(obj, index=NULL,  ...,value) {
            toolkit = obj@toolkit
            .svalue(obj@widget, toolkit, index=index,  ...) <- value
            obj
          })

##' dispatch to toolkit
##' @alias svalue<-
setGeneric(".svalue<-",function(obj, toolkit, index=NULL, ..., value)
           standardGeneric(".svalue<-"))

############### [ ###################################
##' [ method for guiWidget. Passes to .leftBracket
##' 
setMethod("[",
          signature(x="guiWidget"),
          function(x,i,j,...,drop=TRUE) {
            if(missing(drop)) drop <- TRUE
            return(.leftBracket(x@widget, x@toolkit,i,j,...,drop=drop))
          })

##' generic for implementing [ at toolkit level
setGeneric(".leftBracket",function(x, toolkit, i,j, ..., drop=TRUE)
           standardGeneric(".leftBracket"))

################ [<- ##################################
##' base [<- method for guiWidget. Passes to .leftBracket<-
setReplaceMethod("[",signature(x="guiWidget"),
          function(x,i,j,...,value) {
            toolkit = x@toolkit
            if(missing(i) && missing(j))
              .leftBracket(x@widget, toolkit,...) <- value
            else if(missing(j))
              .leftBracket(x@widget, toolkit,i,...) <- value
            else 
              .leftBracket(x@widget, toolkit,i,j,...) <- value
            return(x)
          })

##' generic for .leftBracket<- to implement [<- at toolkit level
setGeneric(".leftBracket<-",function(x, toolkit, i,j, ..., value)
           standardGeneric(".leftBracket<-"))


################## visible ################################
##' Generic to adjust visibility of widget
setGeneric("visible",function(obj, set=NULL, ...) standardGeneric("visible"))

##' visible method for widgets
setMethod("visible",signature(obj="guiWidget"),
          function(obj, set=NULL, ...) {
            toolkit = obj@toolkit
            .visible(obj@widget,toolkit, set=set, ...)
          })
##' dispatch with toolkit
##' @alias visible
setGeneric(".visible",function(obj, toolkit, set=NULL, ...) standardGeneric(".visible"))

############### visible<- ###################################
##' Generic to adjust visibility of widget
setGeneric("visible<-",function(obj, ..., value) standardGeneric("visible<-"))

##' visible<- method for widgets
setReplaceMethod("visible",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .visible(obj@widget, toolkit, ...) <- value
            return(obj)
          })
##' dispatch with toolkit
##' @alias visible<-
setGeneric(".visible<-",function(obj, toolkit,...,value)
           standardGeneric(".visible<-"))

############### enabled ###################################
##' Generic to check if widget is sensitive to user input
setGeneric("enabled",function(obj, ...) standardGeneric("enabled"))

##' Method to check if widget is sensitive to user input
setMethod("enabled",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .enabled(obj@widget, toolkit,...)
          })
##' dispatch with toolkit
##' @alias enabled
setGeneric(".enabled",function(obj, toolkit,...) standardGeneric(".enabled"))

################ enabled<- ##################################
## Generic to set whether widget is sensitive to user input
setGeneric("enabled<-",function(obj, ..., value) standardGeneric("enabled<-"))

## method to adjust whether widget is sensitive to user input
setReplaceMethod("enabled",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .enabled(obj@widget, toolkit,...) <- value
            return(obj)
          })
##' dispatch with toolkit
##' @alias enabled<-
setGeneric(".enabled<-",function(obj, toolkit,...,value)
           standardGeneric(".enabled<-"))

############## size ####################################
##' Generic for size method
setGeneric("size",function(obj, ...) standardGeneric("size"))

##' Method to return preferred size of widget
setMethod("size",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .size(obj@widget, toolkit,...)
          })
##' dispatch with toolkit
##' @alias size
setGeneric(".size",function(obj, toolkit,...) standardGeneric(".size"))

############### sizes<- ###################################
## Generic for method to adjust size of widget
setGeneric("size<-",function(obj, ..., value) standardGeneric("size<-"))

##' Method to adjust size of widget
##'
##' 
setMethod("size<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .size(obj@widget, toolkit,...) <- value
            return(obj)
          })

##' dispatch with toolkit
##' @alias size<-
setGeneric(".size<-",function(obj, toolkit,...,value)
           standardGeneric(".size<-"))



################# font #################################
##' Generic to get the font information
setGeneric("font",function(obj, ...) standardGeneric("font"))
## base method for font generic
setMethod("font",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit <- obj@toolkit
            .font(obj@widget, toolkit,...)
          })

##' dispatch with toolkit
##' @alias font
setGeneric(".font",function(obj,toolkit,...) standardGeneric(".font"))

################# font<- #################################
##' Generic to set font properties of widget
setGeneric("font<-",function(obj, ..., value) standardGeneric("font<-"))

##' base method for setting font properties of a widget
setMethod("font<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit <- obj@toolkit
            .font(obj@widget, toolkit,...) <- value ## DEPRECATED.fixFontMessUp(value)
            return(obj)
          })

##' dispatch with toolkit
##' @alias font<-
setGeneric(".font<-",function(obj, toolkit,...,value)
           standardGeneric(".font<-"))

############## tag ####################################
##' Generic for retrieving data from an object
setGeneric("tag",function(obj,i, drop=TRUE, ...) standardGeneric("tag"))

## base method for retrieving data from a widget
setMethod("tag",signature(obj="guiWidget"),
          function(obj,i,drop=TRUE, ...) {
            toolkit = obj@toolkit
            .tag(obj@widget, toolkit,i, drop=drop,...)
          })
##' dispatch with toolkit
##' @alias tag
setGeneric(".tag",function(obj, toolkit,i, drop=TRUE,...) standardGeneric(".tag"))


################ tag<- ##################################
##' Generic to set arbitrary data in an object
setGeneric("tag<-",function(obj, i, replace=TRUE, ..., value) standardGeneric("tag<-"))

##' base method for assigning data to an object
setMethod("tag<-",signature(obj="guiWidget"),
          function(obj, i, replace=TRUE, ..., value) {
            toolkit <- obj@toolkit
            .tag(obj@widget, toolkit,i, replace, ...) <- value
            return(obj)
          })

##' dispatch with toolkit
##' @alias tag<-
setGeneric(".tag<-",function(obj, toolkit,i, replace=TRUE,...,value)
           standardGeneric(".tag<-"))

################ id ##################################
##' Generic to retrieve id of object
setGeneric("id",function(obj, ...) standardGeneric("id"))
##' Base method to retrieve id from an object
setMethod("id",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit <- obj@toolkit
            .id(obj@widget, toolkit,...)
          })
##' dispatch with toolkit
##' @alias id
setGeneric(".id",function(obj, toolkit,...) standardGeneric(".id"))


################ id<- ##################################
##' Generic to set id for an object
setGeneric("id<-",function(obj, ..., value) standardGeneric("id<-"))

##' base method to set id for an object
setMethod("id<-",signature(obj="guiWidget"),
          function(obj, ..., value) {
            toolkit = obj@toolkit
            .id(obj@widget,toolkit,...) <- value
            return(obj)
          })
##' dispatch with toolkit
##' @alias id<-
setGeneric(".id<-",function(obj, toolkit,...,value)
           standardGeneric(".id<-"))

############## isExtant ####################################
##' generic to check if a widget still exists
setGeneric("isExtant",function(obj, ...) standardGeneric("isExtant"))

##' base method to check if a widget still exists
setMethod("isExtant",signature(obj="guiWidget"),
          function(obj, ...) {
            toolkit = obj@toolkit
            .isExtant(obj@widget,toolkit, ...)
          })

##' dispatch with toolkit
##' @alias isExtant
setGeneric(".isExtant",function(obj, toolkit, ...) standardGeneric(".isExtant"))


################## toolkitProvidesWidget ################################
##' function to check if a toolkit provides a widget
##'
##' @export
##' @param widgetname Character. Name of widget
##' @param toolkit the toolkit to check
##' @returns a logical indicating if the widget is implemented in the toolkit
toolkitProvidesWidget <- function(
                                  widgetname,
                                  toolkit=guiToolkit()){
  .toolkitProvidesWidget (toolkit, widgetname)
}

##' generic for toolkit dispatch
##' @alias toolkitProvidesWidget
setGeneric( '.toolkitProvidesWidget' ,
           function(toolkit,
                    widgetname)
           standardGeneric( '.toolkitProvidesWidget' ))


##' implementation for ANY toolkit
##' @alias toolkitProvidesWidget
setMethod(".toolkitProvidesWidget",
          signature(toolkit="ANY"),
          function(toolkit,
                   widgetname) {
            notThere <- list(guiWidgetsToolkitQt=c("ggraphics","ggraphicsnotebook"),
                             guiWidgestToolkitRGtk2=c("gsvg", "ghtml"),
                             guiWidgetsrToolkitJava=c("gsvg", "ghtml", "ggraphics", "ggraphicsnotebook"),
                             guiWidgetsToolkittcltk=c("gsvg", "ghtml", "ggraphics", "ggraphicsnotebook",
                               "gdfnotebook")
                             )

            notThere <- notThere[[class(toolkit)]]
            return(!widgetname %in% notThere)
          })



##################################################
## Usual R methods made into methods for dispatch

################### update ###############################
##' generic for update
setGeneric("update")

##' base method for update to dispatch on guiWidget instances
setMethod("update",signature(object="guiWidget"),
          function(object, ...) {
            .update(object@widget, object@toolkit, ...)
          })

##' generic for toolkit dispatch
##' @alias update
setGeneric(".update",function(object, toolkit,  ...)
           standardGeneric(".update"))

##' Base method to find length of guiWidget instances
setMethod("length",signature(x="guiWidget"),
          function(x) {
            .length(x@widget, x@toolkit)
          })

##' generic for toolkit dispatch
##' @alias length
setGeneric(".length",function(x, toolkit)
           standardGeneric(".length"))

############## dim ####################################
##' base method to find dim(ension) of guiWidget instances
setMethod("dim",signature(x="guiWidget"),
          function(x) {
            .dim(x@widget, x@toolkit)
          })

##' generic for toolkit dispatch
##' @alias dim
setGeneric(".dim",function(x, toolkit)
           standardGeneric(".dim"))

##' base method to find dimnames attribute of guiWidget instance
#setGeneric("dimnames")
setMethod("dimnames",signature(x="guiWidget"),
          function(x) {
            .dimnames(x@widget, x@toolkit)
          })

##' generic for toolkit dispatch
##' @alias dimnames
setGeneric(".dimnames",function(x, toolkit)
           standardGeneric(".dimnames"))

##' base method to set dimnames for guiWidget instance
#setGeneric("dimnames<-")
setReplaceMethod("dimnames",signature(x="guiWidget"),
                 function(x,value) {
                   .dimnames(x@widget, x@toolkit) <- value
                   return(x)
                 })
##' generic for toolkit dispatch
##' @alias dimnames<-
setGeneric(".dimnames<-",function(x, toolkit, value) {
  standardGeneric(".dimnames<-")
})


## names
## as of 2.5.0 this became primiive
if(as.numeric(R.Version()$major) <= 2 &
   as.numeric(R.Version()$minor) <= 4.1) {
  setGeneric("names")
  setGeneric("names<-")
}

##' Base method for getting names of guiWidget instances
setMethod("names",signature(x="guiWidget"),
          function(x) {
            .names(x@widget, x@toolkit)
          })

##' generic for toolkit dispatch
##' @alias names
setGeneric(".names",function(x, toolkit)
           standardGeneric(".names"))

##' base method to set names of guiWidget instance
setReplaceMethod("names",signature(x="guiWidget"),
                 function(x,value) {
                   .names(x@widget, x@toolkit) <- value
                   return(x)
                 })
##' generic for toolkit dispatch
##' @alias names<-
setGeneric(".names<-",function(x, toolkit, value) {
  standardGeneric(".names<-")
})

##################################################
##' Generic for method to return toolkit widget from guiWidget instance
setGeneric("getToolkitWidget",function(obj) standardGeneric("getToolkitWidget"))

##' base method to return toolkit widget from guiWidget instance
setMethod("getToolkitWidget",signature(obj="guiWidget"),
          function(obj) {
            .getToolkitWidget(obj@widget, obj@toolkit)
          })

##' generic for toolkit dispatch
##' @alias getToolkitWidget
setGeneric(".getToolkitWidget",function(obj, toolkit)
           standardGeneric(".getToolkitWidget"))

