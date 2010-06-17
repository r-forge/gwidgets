## coerce to java/awt/Component
as.jcomponent = function(x) .jcast(x,"java/awt/Component")
as.jstring = function(x) .jnew("java/lang/String",x)
is.rJava = function(x) "jobjRef" %in% class(x)

## convenience
jnew = function(x,...,type="javax/swing/") {
  tmp = paste(type,x,sep="",collapse="")
  .jnew(tmp,...)
}
jcall = function(x,method,...,ret="V")
  .jcall(x,ret,method,...)


## make a jobject
asjobject <- function(x) UseMethod("asjobject")

asjobject.default <- function(x) x

asjobject.character <- function(x)
  .jcast(.jnew("java/lang/String",x),"java/lang/Object")

asjobject.factor <- function(x)
  asjobject(as.character(x))
  

asjobject.numeric <- function(x) {
  if(is.integer(x)) 
    .jcast(.jnew("java/lang/Integer",as.integer(x)),"java/lang/Object")
  else
    .jcast(.jnew("java/lang/Double",x),"java/lang/Object")
}

asjobject.logical <- function(x) 
  .jcast(.jnew("java/lang/Boolean",x), "java/lang/Object")



## return rJava objects from others
getBlock = function(widget) {
  if(is.rJava(widget)) return(widget)
  if(is(widget,"gWidgetrJava")) return(getBlock(widget@block))
  if(is(widget,"guiWidget")) return(getBlock(widget@widget))
  cat("Can't get block")
  return(NA)
}

getWidget = function(widget) {
  while(!is.rJava(widget))
    widget = widget@widget
  return(widget)
}
setMethod(".getToolkitWidget",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gWidgetrJava"),
          function(obj, toolkit) getWidget(obj))



## warning
notImplemented = function(class,method, errstr) {
  cat("Not implemented: ")
  if(!missing(class))
    cat("class", class)
  if(!missing(method))
    cat("has a missing method:",method)
  
  if(!missing(errstr))
    cat(errstr)
  cat("\n")
}
