## Handler code
## allHandlers is keyed by ID,type,handlerID then flag, a logical
## ID, type, handlerID are characters


## use actualobj to pass something different to h$obj in handlers

## see addHandkerKeystroke for example where FUN is passed in so that
## arguments such as %K can be realized (for KeyRelease events)
## see bind man for key codes
### This stores the handlers
### it will be hash keyed by ID and then handlerID.
### The ID allows for garbage collection (sometime)
allHandlers = list()
## list with type [[ID]][[type]][[handlerID]]
assignInNamespace("allHandlers",list(),ns="gWidgetstcltk")

setMethod(".addHandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   signal, handler, action=NULL, ...) {
            ## use tkbind
            ## What to do for an ID, how to deregister?
            ID = as.character(obj@ID)

            theArgs = list(...)
            actualobj <- obj
            if(!is.null(theArgs$actualobj))
              actualobj <- theArgs$actualobj
            ## theArgs may have an extra with name=key, value
            FUN <- theArgs$FUN
            handler <- force(handler)
            if(is.null(FUN)) {
              tkbind(getWidget(obj),signal,
                     function(...) {
                       h = list(
                         obj=actualobj,
                         action=action)
                       handler(h,...)
                     })
            } else {
              tkbind(getWidget(obj), signal,FUN)
            }

            ## We use store the handlers. In this case, we only use the signal
            ## for remove handler. No block,unblock
            allHandlers <- getFromNamespace("allHandlers",ns="gWidgetstcltk")
            
            if(length(allHandlers[[ID]][[signal]]) == 0)
              handlerID <- "1"
            else
              handlerID <- as.character(length(allHandlers[[ID]][[signal]]) + 1)
            
            
            allHandlers[[ID]][[signal]][[handlerID]]$flag <- TRUE
            assignInNamespace("allHandlers",allHandlers,ns="gWidgetstcltk")
            
            ## ID
            invisible(list(type=signal,handlerID=handlerID))
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


setMethod(".removehandler",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit, ID=NULL, ...) {
            ## ID here has two components
            type = ID$type
            handlerID=ID$handlerID
            if(is.null(type) || is.null(handlerID)) return()
            
            ID = as.character(obj@ID)


            ## set flag to be FALSE, then remove
            allHandlers = getFromNamespace("allHandlers",ns="gWidgetstcltk")
            allHandlers[[ID]][[type]][[handlerID]]$flag <- FALSE
            ## now store the hash
            assignInNamespace("allHandlers",allHandlers,ns="gWidgetstcltk")

            ## idle is handled differently, via flag<-FALSE
            ## seee FAQ 3.2 for this
            if(type != "idle") { 
              .addHandler(obj,toolkit,signal=type,handler = function(...) {}) # blank functin
            }
          })

setMethod(".addhandleridle",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gWidgettcltk"),
          function(obj, toolkit,
                   handler=NULL, action=NULL, interval=1000, ...) {

            ID = as.character(obj@ID)
            allHandlers = getFromNamespace("allHandlers",ns="gWidgetstcltk")
            if(length(allHandlers[[ID]]) == 0)
              handlerID = "1"
            else
              handlerID = as.character(length(allHandlers[[ID]]) + 1)

            allHandlers[[ID]][["idle"]][[handlerID]]$flag <- TRUE
            assignInNamespace("allHandlers",allHandlers,ns="gWidgetstcltk")

            ## use tcl("apply",time, function) in a while loop here
            h = list()
            h$obj=obj
            h$action=action
            
            f <- function() {
              if(!windowExists(obj)) return() # otherwise, issue when destroyed
              handler(h)
              allHandlers = getFromNamespace("allHandlers",ns="gWidgetstcltk")
              flag = allHandlers[[ID]][["idle"]][[handlerID]]$flag
              if (flag)
                tcl("after", interval, f)
            }
            ## start it off
            f()
            ## ID
            invisible(list(type="idle",handlerID=handlerID))
          })





