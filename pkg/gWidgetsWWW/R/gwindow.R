##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/


## XXX would like to integrate layout manager into this
## Should be a big panel with menubar and toolbar and statusbar areas
## ... contains width=, height= for gsubwindow call
## if container is not null, then a subwindow is made
## handler called on unload

##' Main top level window
##'
##' Each script needs to have one and only one instance as a global
##' variable. The visible argument is ignored. One must call
##' \code{visible<-} with a value of \code{TRUE} after the page is
##' layed out. (This prints the javascript to the browser).
##'
##' @param title Page title
##' @param visible ignored. Must set visibility TRUE after GUI construction is done.
##' @param width size in in pixels. (Mostly only for a subwindow)
##' @param height size in pixels (For subwindows)
##' @param parent If non-\code{NULL} creates a sub window.
##' @param handler assigned to page unload event
##' @param action passed to handler
##' @param ... ignored
##' @return a gwindow instance. Many methods but key one is \code{visible<-}.
##' @note There are some \pkg{proto} properties that can be set to adjust the values. These are \code{loadingText}; \code{doLoadingText}; \code{..show\_error\_message}; \code{AJAXtimeout}
##' @examples
##' \dontrun{
##' w <- gwindow("hello test")
##' gbutton("Click me", cont=w, handler=function(h,...) galert("Hello world", parent=w))
##' visible(w) <- TRUE
gwindow <- function(title="title", visible=TRUE,
                    name=title,
                    width = NULL, height = NULL, parent = NULL,
                    handler=NULL, action=NULL,...) {

  ## width, height  for subwindows
  ## changed container argument to  parent to match gWidgets
  container <- parent

   ## make a subwindow?
   if(!is.null(container))
     return(.gsubwindow(title=title,handler=handler, action=action,
                        visible=visible,
                        width = width, height = height,
                        container=container,...))

  w <- EXTContainer$new(
                        visible=visible,
                        ..actions = list())
  class(w) <- c("gWindow",class(w))
  
  ## no parent container -- so no ID. We fix this
  w$ID <- "gWidgetID0"                  # XXX Issue if more than one per page!
  w$sessionID <- makeSessionID()
  w$toplevel <- w
  w$..renderTo <- String("Ext.getBody()") # can override
  w$..show_error_messages <- gWidgetsWWWIsLocal()         # set to NULL to not show
  w$doLoadingText <- gWidgetsWWWIsLocal() # do we print a message when calling a handler
  w$loadingText <- gettext("Loading...")  # prints when a handler is called to indicate a request.
  ##  w$..visible <- FALSE



  w$setValue(value=title)
  w$jscriptHandlers <- list()        # handlers in parent winoow
  w$JSQueue <- character()           # output of JS handlers
  w$toplevel <- w
  w$..IDS <- c()
  w$..blocked_handlers <- c()           # IDs of handlers not to call
  theArgs <- list(...)
  w$..AJAXtimeout <- ifelse(is.null(theArgs$AJAXtimeout), 10000, theArgs$AJAXtimeout)

  ## store name in title for handlers.
  w$titlename <- make.names(title)
  assign(w$titlename,w, envir=.GlobalEnv)

  ## Find values from apache config or from local config

  ## find URL for AJAX call, place into toplevel for later reference
  if(!exists("gWidgetsWWWAJAXurl") || is.null(gWidgetsWWWAJAXurl))
    gWidgetsWWWAJAXurl <- getOption("gWidgetsWWWAJAXurl")
  if(is.null(gWidgetsWWWAJAXurl))  {
    gWidgetsWWWAJAXurl <- "/gWidgetsWWW"
  }
  w$..gWidgetsWWWAJAXurl <- gWidgetsWWWAJAXurl
  w$..gWidgetsWWWrunUrl <- getWithDefault(getOption("gWidgetsWWWrunUrl"), "/gWidgetsWWWrun")
  #### methods ####
  
  ##' run a handler
  ##' id is id of handler. Can be used for blocked handlers
  ##' context is named list of values to pass into "h" object
  ##' Handlers that don't run raise an error
  w$runHandler <- function(., id, context) {
    id <- as.numeric(id)
    if(! (id %in% .$..blocked_handlers)) {
      lst <- .$jscriptHandlers[[as.numeric(id)]]
      h <- list(obj=lst$obj, action = lst$action)
      if(!missing(context) &&  is.list(context)) {
        for(i in names(context))
          h[[i]] <- context[[i]]
      }
      ## XXX changed to use queue. Each handler should
      ## Each XXXJS call  adds to the JSQueue, it isn't done here
      out <- try(lst$handler(h), silent=TRUE)          # add to JS Queue
#      return(lst$handler(h))
    }
    if(inherits(out, "try-error"))
      stop(sprintf("<br />Error running handler: %s", out))
    .$runJSQueue()                      # run the queue
  }


  
  ## for top-level window visible same as print
  w$setVisible <- function(., value) {
    ## same as print
    if(value) {
      .$Show()
    } else {
      cat("can't hide top-level window\n", file=stdout())
    }
  }

  w$addAction <- function(., action) 
    .$..actions <- c(.$..actions, action)
  
  ## set title
  w$setValueJS <- function(.) {
    if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
     
     out <- String() +
       'document.title = ' + shQuote(.$..data) +';'
     return(out)
   }
   
   
   ## css and scripts are different for gwindow instances, as these
   ## are the toplevel instances -- sub classes place there values into
   ## these lists.
   ## css is a list keyed by class.
   w$css <- list("gWindow"=list(obj=w,FUN=function(.) {return("")}))
   
   w$scripts <-
     list("gWindow" = list(obj=w,
            FUN = function(.) {
              ## Scripts, this gets placed into a
              ## list keyed by the class of the object
              
              out <- String()

              ## add Library and Style Sheets
              ## use "script" to add library
              ## use "link" to add style sheet for type
##               out <- out +
##                 'AddLibrary = function(type, file){' +
##                   'var NewNode=document.createElement(type);' +
##                     'NewNode.src=file;' +
##                        'document.body.appendChild(NewNode);' +
##                          '};' + '\n'
              

              
              ## runHandlerJS is key to linking in R with the web page
              ## we pass in an ID and optionally some values with keys.


              ## Some javascript functions
              ## XXX make better

              if(.$has_local_slot("..show_error_messages")) {
                processFailure <- paste("function processFailure(response, options) {",
                                        "Ext.example.msg('Error:', response.responseText, 4);",
                                        "};",
                                        sep="\n")
              } else {
                processFailure <- paste("function processFailure(response, options) {",
                                        "eval(response.responseText);",
                                        "};",
                                        sep="\n")                
              }
              out <- out +
                processFailure +
                  "\n" +
                    "function evalJSONResponse(response, options) {" +
                      "eval(response.responseText);" +
                        "};" + "\n"

              ## code to run a javascript handler
              out <- out +
                'runHandlerJS = function(id,context) {' +
                  ifelse(.$doLoadingText, 
                         sprintf("Ext.getBody().mask('%s');", .$loadingText),
                         "") + "\n" +
                  "Ext.Ajax.request({" +
                    "url: '" + .$..gWidgetsWWWAJAXurl + "'," +
                      "success: evalJSONResponse," +
                        "failure: processFailure," +
                          "method: 'POST', " +
                            "timeout:" + .$..AJAXtimeout + "," +
                              "params: { type: 'runHandler', " +
                                "sessionID: sessionID," +
                                  "id: id," +
                                    "context: context" +
                                        #                                    "extra: extra" +
                                      "}" +
                                        "}); " +
                                          '};' + "\n"

              ## show loading text box if requested
              if(.$doLoadingText) {
                out <- out +
                  "Ext.Ajax.on('requestcomplete', function() {Ext.getBody().unmask() }, this);" +
                    "Ext.Ajax.on('requestexception', function() {Ext.getBody().unmask()}, this);" + "\n"
              }
              
              ## transportToR copies data in widget back into R
              ## using a global variable IDXXX
              ## We don't expect a return value
              out <- out +
                '_transportToR = function(id, val) {' +
                  "Ext.Ajax.request({" +
                    "url: '" + gWidgetsWWWAJAXurl + "'," +
## JV                     "success: evalJSONResponse," +
                      ## we get some XML back, not JSON
                      "success: function(response, opts) {}," +
                        "failure: processFailure," +
                          sprintf("timeout: %s,", .$..AJAXtimeout) +
                            "method: 'POST'," +
                              "params: { type: 'assign', " +
                                "sessionID: sessionID," +
                                  "variable: id," +
                                    "value: val" +
                                      "}})};" + "\n"
              
              out <- out +
                'function clearSession() {' +
                  "Ext.Ajax.request({" +
                    "url: '" + gWidgetsWWWAJAXurl + "'," +
                      "method: 'POST'," +
                        "params: { type: 'clearSession', " +
                          "sessionID: sessionID" +
                            "}})};" + "\n"
              

              
              ## this is for tooltips in, say, gnotebook tabs.
              out <- out +
                "Ext.QuickTips.init();\n"

              return(out)
            })
          )

   w$ExtConstructor <- "Ext.Panel" ## inherits
   w$ExtCfgOptions <- function(.) { ## ih
     out <- list(
                 renderTo= .$..renderTo,
                 border = TRUE,
#                 bodyBorder = FALSE,
                 hideBorders = FALSE,
                 autoScroll = TRUE
                 )
     
     return(out)
   }

   ## code to set up iconclasses for use with buttons, toolbar, menubar
   w$iconDir <- ""
   w$makeIconClasses <- function(.) {
     ## XXX -- this doesn't work with IE so we cut it out.
     return("")
     ## old below
     out <- String()
     x <- getStockIcons();
     nms <- names(x)
     for(i in 1:length(x)) {
       out <- out +
         'Ext.util.CSS.createStyleSheet("' +
           paste("button.",nms[i], "{background-image:url(",x[i],")};", sep="",
                 collapse="") +
           '");' + '\n'
     }

     return(out)
   }
   
   w$header <- function(.) {
     out <- String() + .$makeIconClasses()
     .$Cat(out)
   }

   
  w$footer <- function(.) {
  
     ## clear out any old IDS
     remove(list=ls(pat="^gWidgetID",envir=.),envir=.)

     ## doLayout()
     out <- String() +
       .$asCharacter() + '.doLayout();' + '\n'

     ## set title 
     out <- out +
       'document.title =' +shQuote(.$getValue()) + ';\n' 

     ## write out sessionID
     out <- out +
       'var sessionID =' + shQuote(.$sessionID) + ';' + '\n'
     
     .$Cat(out)


   }

  ## can't dispose of top-level window
  w$visible <- function(.) {}

  w$..setHandlers <- function(.) {
     ## deprecated, see addHandler now
     return("")
   }

  ## handlers for proxy stores
  ## These methods are used by gbigtable and by gtree to dynamically populate data

  ##' Property. We need to look up proxy stores when we process
  w$proxyStores <- list()

  ##' add a proxy store for later lookup
  w$addStore <- function(., store) {
    l <- .$proxyStores
    l[[store$asCharacter()]] <- store
    .$proxyStores <- l
  }

  ##' get a proxy store from its id
  w$getStoreById <- function(., id) {
    .$proxyStores[[id]]
  }

  
  
  ## unload handler
  if(!is.null(handler)) 
    w$addHandler("onunload",handler, action=action)

   
    invisible(w)
 }

## gsubwindow
## a subwindow appears on top of a regular window
## style properties width, height (size) and x, y are used
## should be draggable -- but doesn't seem to work
##
## svalue<- sets title
## visible(obj) TRUE is showing
## visible(obj) <- FALSE/TRUE hides/shows
.gsubwindow <- function(title="Subwindow title", visible=TRUE,
                        width=500, height=300,
                        handler = NULL, action=NULL, container=NULL,...) {

  widget <- EXTContainer$new(toplevel=container$toplevel,
                             ..visible = as.logical(visible)
                             ) 
  class(widget) <- c("gSubwindow",class(widget))
  widget$setValue(value=title)
  
  if(is.null(width)) width <- 500
  if(is.null(height)) height <- 300
  widget$..style <- c("width"=width, height=height)
  ## methods
  widget$addAction <- function(., action) 
     .$..actions <- c(.$..actions, action)


  widget$dispose <- function(.) visible(.) <- FALSE

  ## set title
  widget$setValueJS <- function(.) {
  if(.$has_local_slot("..setValueJS"))
    .$..setValueJS(...)
  
  out <-  sprintf("%s.setTitle(%s);\n", .$asCharacter(), shQuote(escapeHTML(.$..data)))
  .$addJSQueue(out)
  }


  ## visible
  widget$setVisible <- function(., value) {
    value <- as.logical(value)
    .$..visible <- value
    if(.$has_local_slot("..shown"))
      .$addJSQueue(.$setVisibleJS())
    else 
      .$Show(queue=TRUE)
  }

  ## odd inheritance here
  widget$setVisibleJS <- function(.) {
    if(.$has_local_slot("..setVisibleJS"))
      .$..setVisibleJS()
    
    ## opposite -- we already changed if we got here
    out <- sprintf("%s.%s();", .$asCharacter(),
                   ifelse(.$..visible, "show", "hide"))
    .$addJSQueue(out)
  }


  widget$ExtConstructor = "Ext.Window"
  widget$ExtCfgOptions <- function(.) {
    style <- .$..style
    width <- style['width']
    height <- style['height']
    
    out <- list(
                'title' = .$..data,
                'layout' = "auto",      # not fit
                'width' = as.numeric(width),
                'height' =  as.numeric(height),
                'closeAction' = "hide",
                'autoScroll' = TRUE,
                'plain' = TRUE,
                'button' = String('[{text: "Close", handler: function(){') +
                'o' + .$ID + '.hide();}}]'
                )
    ## statusbar. Menu? Tool?
    if(exists("..statusBar",envir=., inherits=FALSE)) {
      sbText <- String() +
        'new Ext.ux.StatusBar({' +      # as of 3.0 not in ext -- uses toolbar instead
          'id: "' + .$ID + 'statusBar",' +
            'defaultText: "",' +
              'text:' + shQuote(.$..statusBarText) +
                '})'
      out[['bbar']] <- sbText
      .$..statusBar$..shown <- TRUE
    }
    
    return(out)
  }

  

  widget$header <- function(.) {}
  widget$footer <- function(.) {
    ## render and show
    out <- String() +
      sprintf("%s.render();", .$asCharacter())
    if(visible(.))
      out <- out + sprintf("%s.show();", .$asCharacter())

    out
  }

  ## we don't add. The subwindow prints out
  container$add(widget, ...)


  ## return
  invisible(widget)
}
