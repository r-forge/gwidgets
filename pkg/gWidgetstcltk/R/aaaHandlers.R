## Handler code
## redid 7/2010

##' run handlers for this signal
##'
##' @param obj is gWidgets object
##' @param signal the signal (handler list keyed by signal)
##' @param h list with proper components from call
runHandlers <- function(obj, signal, h, ...) {
  l <- tag(obj, "..handlers")
  signalList <- l[[signal]]
  sapply(signalList, function(i) {
    if(!i$blocked) {
      i$handler(h, ...)
    }
  })
}

##' add a handler to an object
##'
##' The basic idea is that a list of handlers (keyed by the signal) is kept along with a flag
##' indicating whether the handler is blocked or not
##' The binding is done to call the runHandlers function so that this flag can be intercepted
setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            ## use tkbind

            l <- tag(obj, "..handlers")
            if(is.null(l))
              l <- list()

            signalList <- l[[signal]]
            if(is.null(signalList))
              signalList <- list()


            ## each component of signalList is a list with ID, blocked, handler, action=action
            id <- digest(Sys.time())    # some unique key
            hList <- list(ID=id,
                          blocked=FALSE,
                          handler=handler,
                          action=action)
            
            signalList[[length(signalList) + 1]] <- hList # append
            l[[signal]] <- signalList
            tag(obj, "..handlers") <- l

            id <- list(id=id, signal=signal) # need this to block/remove/unblock
            
            theArgs = list(...)

            actualobj <- getWithDefault(theArgs$actualobj, obj)

            ## theArgs may have an extra with name=key, value
            FUN <- theArgs$FUN
            handler <- force(handler)
            if(is.null(FUN)) {
              tkbind(getWidget(obj), signal,
                     function(...) {
                       h = list(
                         obj=actualobj,
                         action=action)

                       runHandlers(obj, signal, h, ...)
#                       handler(h,...)
                     })
            } else {
              ## when does this get called?
              stop("DEBUG: Call addHandler with FUN")
              tkbind(getWidget(obj), signal,FUN)
            }

            ## return id
            invisible(id)
          })

## for tcltk objects
setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="tcltkObject"),
          function(obj, toolkit, signal, handler, action=NULL, ...) {

            theArgs = list(...)
            theobj = theArgs$actualobj

            handler <- force(handler)
            tkbind(obj,signal, function(...) {
              h = list(obj=theobj, action=action)
              handler(h,...)
            })
            invisible(list(type=NULL,handlerID=NULL))
          })

##' idle handler is different == have this hack to keep calling "after"
setMethod(".addhandleridle",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler=NULL, action=NULL, interval=1000, ...) {

            signal <- "idle"
            
            l <- tag(obj, "..handlers")
            if(is.null(l))
              l <- list()
            
            signalList <- l[[signal]]
            if(is.null(signalList))
              signalList <- list()
            
            
            ## each component of signalList is a list with ID, blocked, handler, action=action
            id <- digest(Sys.time())    # some unique key
            hList <- list(ID=id,
                          blocked=FALSE,
                          handler=handler,
                          action=action)
            
            signalList[[length(signalList) + 1]] <- hList # append
            l[[signal]] <- signalList
            tag(obj, "..handlers") <- l
            
            
            ## use tcl("apply",time, function) in a while loop here
            h = list()
            h$obj=obj
            h$action=action
            
            f <- function() {
              if(!windowExists(obj))
                return() # otherwise, issue when destroyed

              l <- tag(obj, "..handlers")
              sigList <- l[['idle']]
              ind <- sapply(sigList, function(i) i$ID == id)
              if(any(ind)) {
                if(!sigList[[which(ind)]]$blocked)
                  sigList[[which(ind)]]$handler(h)
                tcl("after", interval, f)
              }
            }
            ## start it off
            f()
            ## ID
            id <- list(id=id, signal="idle")
          })


##' Function to call to update the "blocked" flag on a handler. runHandlers consults this
##' before making the call
.blockUnblock <- function(obj, ID, block=TRUE, ...) {
  l <- tag(obj, "..handlers")
  
  if(is.null(ID)) {
    ## do all IDS
    sapply(names(l), function(signal) {
      sigList <- l[[signal]]
      if(length(sigList)) {
        for(i in sigList)
          .blockUnblock(obj, list(id=i$ID, signal=signal), block, ...)
      }
    })
    return()
  } else {
    id <- ID$id
    signal <- ID$signal
    if(is.null(id) || is.null(signal))
      return()
    
    if(is.null(l[[signal]]))
      return()                 # no signal list
    ind <- sapply(l[[signal]], function(i) {
      i$ID == id
    })
    
    if(!any(ind))
      return()   
    
    for(i in which(ind)) {
      l[[signal]][[i]]$blocked <- block
    }
  }
  
  tag(obj, "..handlers") <- l
}

##' call to block a handler by ID. If ID=NULL, all handlers are blocked
setMethod(".blockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            .blockUnblock(obj, ID, block=TRUE)
          })

##' call to unblock a handler by ID. If ID=NULL, all handlers are unblocked
setMethod(".unblockhandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            .blockUnblock(obj, ID, block=FALSE)            
          })

##' method to remove a handler
setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ID=NULL, ...) {

            if(is.null(ID)) {
              ## remove all handlers. Call this recursively
              l <- tag(obj, "..handlers")
              sapply(names(l), function(signal) {
                sigList <- l[[signal]]
                if(length(sigList)) {
                  for(i in sigList)
                    .removehandler(obj, toolkit, ID=list(id=i$ID, signal=signal))
                }
              })
            } else {
              ## ID here has two components: id, signal
              id <- ID$id
              signal <- ID$signal
              
              if(is.null(id) || is.null(signal))
                return()
              
              ## ## idle is handled differently, via flag<-FALSE
              ## ## seee FAQ 3.2 for this
              ## if(signal == "idle") { 
              ##   .addhandleridle(obj, toolkit, signal=type, handler = function(...) {}) # blank functin
              ##   return()
              ## }
              
              
              l <- tag(obj, "..handlers")
              if(is.null(l[[signal]]))
                return()                 # no signal list
              ind <- sapply(l[[signal]], function(i) {
                i$ID == id
              })
              
              if(!any(ind))
                return()                  # no match on id
              
              for(i in which(ind))
                l[[signal]][[i]] <- NULL
              tag(obj, "..handlers") <- l
            }
          })
          




