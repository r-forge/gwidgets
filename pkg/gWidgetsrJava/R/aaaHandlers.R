### This stores the handlers
### it will be ahsh keyed by ID and then handlerID.
### The ID allows for garbage collection (sometime)
allHandlers = list()
assignInNamespace("allHandlers",list(),ns="gWidgetsrJava")


## Change this
runHandlerFor = function(ID,type, ...) {

  allHandlers = getFromNamespace("allHandlers",ns="gWidgetsrJava")


  lst = allHandlers[[as.character(ID)]][[type]]
  if(!is.null(lst)) {
    for(i in lst) {
      handler = i$handler; lst$handler <- NULL
      envir = i$envir; lst$envir=NULL
      do.call("handler",list(i),envir=envir)
    }
  } else {
#    cat("couldn't find handler\n")
  }
}



## Key function. First it figures out handlerID, then dispatches

addJHandler = function(obj, 
  handler = NULL, action = NULL, type, event, class,
  cast=NULL,                            # cast for jobj
  jobj = getWidget(obj),                # override for jobj
  ...)  {

  theArgs <- list(...)
  
  ID = as.character(obj@ID)
  handlerID = "1"                         # unless otherwise
  allHandlers = getFromNamespace("allHandlers",ns="gWidgetsrJava")

  if(is.null(allHandlers[[ID]])) {
    allHandlers[[ID]] <- list()
  }

  if(is.null(allHandlers[[ID]][[type]])) 
    allHandlers[[ID]][[type]] <- list()


  if(is.null(handler)) return(NA)


  
   d <- .jcall("gWidgetsrJava/ActionDispatcher",
               "LgWidgetsrJava/ActionDispatcher;",
               "getGlobalDispatcher")
   r <- .jcall(d,"S","add",.jcast(jobj,"java/lang/Object"),
               .jnew("java/lang/Integer",ID)
               )
##               ,.jnew("java/lang/Integer",handlerID)
##               )
  
  ## for button
  ## type="addActionListener"
  ## class = "java/awt/event/Actionlistener")
  ## cast=javax/swing/AbstractButton"

  if(is.null(cast))
    .jcall(jobj,"V",type,.jcast(d,class))
  else
    .jcall(.jcast(jobj,cast),"V",type,.jcast(d,class))
  
  ## now add handler to handler environment
  handlerID = length(allHandlers[[ID]][[event]]) + 1
  allHandlers[[ID]][[event]][[handlerID]] = list(
                             handler=handler,
                             obj = if(is.null(theArgs$actualobj)) obj else theArgs$actualobj,
                             action=action,
                             envir=parent.frame()
                             )

  ## now store the hash
  assignInNamespace("allHandlers",allHandlers,ns="gWidgetsrJava")
  
  ## this ID allows one to disassociate a handler
  return(c(type,handlerID))
  
}

removeHandler = function(obj, handlerID) {
  ID = as.character(obj@ID)
  allHandlers = getFromNamespace("allHandlers",ns="gWidgetsrJava")
  allHandlers[[ID]][[handlerID]] <- NULL
  assignInNamespace("allHandlers",allHandlers,ns="gWidgetsrJava")
}


## code for handlers goes here
## this is lifted from widgets.R in iWidgets




.handlers<-new.env()
.names<-new.env()
.root<-new.env()

## function to dispatch
.dispatch.event.for <- function(name,...) {
  cat(".dispatch event for ",name,"\n")
    h<-try(get(name,envir=.handlers),TRUE)
    if (inherits(h,"iHandler")) h$handler(h,...)
}




