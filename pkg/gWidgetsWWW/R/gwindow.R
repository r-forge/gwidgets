## XXX would like to integrate layout manager into this
## Should be a big panel with menubar and toolbar and statusbar areas
## ... contains width=, height= for gsubwindow call
## if container is not null, then a subwindow is made
## handler called on unload

gwindow <- function(title="title",file="",visible=TRUE,
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

   w <- EXTContainer$new(file=file,
                         visible=visible,
                         ..actions = list())
   class(w) <- c("gWindow",class(w))

  ## no parent container -- so no ID. We fix this
  w$ID <- "gWidgetID0"                  # XXX Issue if more than one per page!
  w$sessionID <- makeSessionID()
  w$toplevel <- w
  w$..renderTo <- String("Ext.getBody()") # can override
  w$..show_error_messages <- TRUE         # set to NULL to not show
  w$doLoadingText <- gWidgetsWWWIsLocal() # do we print a message when calling a handler
  w$loadingText <- gettext("Loading...")  # prints when a handler is called to indicate a request.
  ##  w$..visible <- FALSE
  
  w$setValue(value=title)
  ## XXX handle cat to STDOUT
   w$file <- file; unlink(file)
  
  w$jscriptHandlers <- list()        # handlers in parent winoow
  w$toplevel <- w
  w$..IDS <- c()
  w$..blocked_handlers <- c()           # IDs of handlers not to call


  ## store name in title for handlers.
  w$titlename <- make.names(title)
  assign(w$titlename,w, envir=.GlobalEnv)
  
   
  #### methods ####
  ##' run a handler
  ##' id is id of handler. Can be used for blocked handlers
  ##' context is named list of values to pass into "h" object
  w$runHandler <- function(., id, context) {
    id <- as.numeric(id)
    if(! (id %in% .$..blocked_handlers)) {
      lst <- .$jscriptHandlers[[as.numeric(id)]]
      h <- list(obj=lst$obj, action = lst$action)
      if(!missing(context) &&  is.list(context)) {
        for(i in names(context))
          h[[i]] <- context[[i]]
      }
      return(lst$handler(h))
    }
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
              ## XXX make betters

              if(exists("..show_error_message", envir=.)) {
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

              if(!exists("gWidgetsWWWAJAXurl") || is.null(gWidgetsWWWAJAXurl))  {
                gWidgetsWWWAJAXurl <- "/gWidgetsWWW"
              }
              
              ## XXX Need to fix
              ## * url is fixed
              ## key, extra needs to be added
              out <- out +
                'runHandlerJS = function(id,context) {' +
                  ifelse(.$doLoadingText, 
                         sprintf("Ext.getBody().mask('%s');", .$loadingText),
                         "") + "\n" +
                  "Ext.Ajax.request({" +
                    "url: '" + gWidgetsWWWAJAXurl + "'," +
                      "success: evalJSONResponse," +
                        "failure: processFailure," +
                          "method: 'POST', " +
                            "timeout: 2000," +
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
                          "timeout: 2000," +
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
              

              ## put into run function
##               out <- out +
##                 'clearSession = function() {' +
##                   "Ext.Ajax.request({" +
##                     "url: '" + gWidgetsWWWAJAXurl + "'," +
##                       "success: evalJSONResponse," +
##                         "failure: processFailure," +
##                           "method: 'POST'," +
##                             "params: { type: 'clearSession', " +
##                               "sessionID: sessionID," +
##                                     "}})};"

              
              
              ## this is for tooltips in, say, gnotebook tabs.
              out <- out +
                "Ext.QuickTips.init();\n"

              ## ext message for galert
              f <- system.file("javascript","ext.ux.example.js", package="gWidgetsWWW")
              out <- out + paste(readLines(f, warn=FALSE), sep="\n") 

              ## statusbar
              if(exists("..statusBar", envir=., inherits=FALSE)) {
                f <- system.file("javascript","ext.ux.statusbar.js", package="gWidgetsWWW")
                out <- out + paste(readLines(f, warn=FALSE), collapse="\n") 
              }

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
     
##      out <- out +
##        'o' + .$ID + '.on("destroy", function() {clearSession("hi");});'

     ## finish Ext.onReady
#     out <- out + '})\n'
     .$Cat(out)


   }

  ## can't dispose of top-level window
  w$visible <- function(.) {}

  w$..setHandlers <- function(.) {
     ## deprecated, see addHandler now
     return("")
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

  ## visible
  widget$setVisible <- function(., value) {
    .$..visible <- as.logical(value)
    if(exists("..shown",envir=., inherits=FALSE)) {
      cat(.$setVisibleJS(), file=stdout())
    } else {
      if(as.logical(value))
        print(.)
    }
  }

  widget$dispose <- function(.) visible(.) <- FALSE

  ## set title
  widget$setValueJS <- function(.) {
  if(exists("..setValueJS", envir=., inherits=FALSE)) .$..setValueJS(...)
  
    out <-  String() +
      'o' + .$ID + '.setTitle(' + shQuote(.$..data) + ');' + '\n'
    cat(out, file=stdout())
  }
  ## odd inheritance here
  widget$setVisibleJS <- function(.) {
    if(exists("..setVisibleJS", envir=., inherits=FALSE))
      .$..setVisibleJS()
    
    ## opposite -- we already changed if we got here
    method <- ifelse(.$..visible, "show", "hide")
    out <- String() + 
      'o' + .$ID + '.' + method + '();'
    cat(out, file=stdout())
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
#        'new Ext.StatusBar({' +
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
    out <- String()

    out <-  out + 'o' + .$ID + '.render();' + '\n'
    if(visible(.))
      out <-  out + 'o' + .$ID + '.show();' + '\n'

    .$Cat(out)
  }

  ## we don't add. The subwindow prints out
  container$add(widget, ...)


  ## return
  invisible(widget)
}
