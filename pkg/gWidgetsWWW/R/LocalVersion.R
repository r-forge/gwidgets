##' Environment to hold different sessions
assign("..gWidgets_sessionEnv", new.env(), envir=.GlobalEnv)

##' remove session form list to free up space
clearSessionId <- function(ID) {
  sessionEnv <- get("..gWidgets_sessionEnv",  envir=.GlobalEnv)
  sessionEnv[[ID]] <- NULL
  assign("..gWidgets_sessionEnv", sessionEnv, envir=.GlobalEnv)
}

##' This lists the gwindow objects and matches against ID
getBaseObjectFromSessionID <- function(sessionID, envir=.GlobalEnv) {
  sessionEnv <- get("..gWidgets_sessionEnv",  envir=.GlobalEnv)  
  return(sessionEnv[[sessionID]])
  ## XXX delete me
  ## get the gWindow instance matching session ID
  ## return object or NULL
  gWindowObjects <- getBaseObjectsFromEnvironment(envir)

  if(length(gWindowObjects)) {
    for(i in gWindowObjects) {
      w <- get(i, envir=envir)
      if(w$sessionID == sessionID) {
        return(w)
      }
    }
  }
  return(NULL)
}

##' return all gWindow instances
getBaseObjectsFromEnvironment <- function(envir=.GlobalEnv) {
  vars <- ls(envir=envir)
  ind <- sapply(vars, function(i) inherits(get(i, envir=envir), "gWindow"))
  if(any(ind))
    gWindowObjects <- vars[ ind ]
  else
    gWindowObjects <- character(0)
  return(gWindowObjects)
}
  
  
escapeBrackets <- function(x) gsub("(\\{|\\})", "\\\\\\1", x)


##' From dynamicHelp in tools:
##'
##' Find mime type from file extension
##' (Taken from Duncan Murdoch's work)
mime_type <- function(path) {
  ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
  if(n <- length(ext)) ext <- ext[n] else ""
  switch(ext,
         "css" = "text/css",
         "gif" = "image/gif", # in R2HTML
         "jpg" = "image/jpeg",
         "svg"="image/svg+xml",
         "html" = "text/html",
         "js" = "text/javascript",
         "pdf" = "application/pdf",
         "eps" = "application/postscript",
         "ps" = "application/postscript", # in GLMMGibbs, mclust
         "sgml"= "text/sgml", # in RGtk2
         "xml" = "text/xml",  # in RCurl
         "text/plain")
}

##' return an error
##' @param msg String. error message
##' @param status_code Integer. use to specify status code. Default if 404L
makeErrorPage <- function(msg, status_code=400L) {
  list(payload=paste(msg, collase="\n"),
       "content-type"="text/html",
       "status code"=status_code)
}

##' Process a script through source and capture ouutput
##'
##' @param file filename of script
##' @param content_type MIME type of output
processScript <- function(file, content_type="text/html") {
  ## source file, capture output
  out <- capture.output(source(file))

  list(payload=paste(out, collapse="\n"),
       "content-type" = mime_type(file),
       "headers" = NULL,
       "status code"=200L
       )
}
  


##' process file from basehtml (ext/, images/, ...)
##' @param path path to file
##' @param query query string (path?var1=x&var2=u) -> path=path; query=c(var1="x", var2="u")
##' @param ... ignored
##' 
processBasehtmlFile <- function(path, query, ...) {
  f <- sprintf("/basehtml/%s", paste(path, collapse="/"))
  f <- gsub("//","/", f)                # just in case
  f <- system.file(f, package="gWidgetsWWW")

  if(!file.exists(f))
    return(makeErrorPage(sprintf("Can't find %s", f)))
  
  list(file=f, 
       "content-type" = mime_type(f),
       "headers" = NULL,
       "status code"=200L
       )

}

##' Stub to inspect path, query, ... values
##' @param path path to file
##' @param query query string (path?var1=x&var2=u) -> path=path; query=c(var1="x", var2="u")
##' @param ... ignored
processPath <- function(path, query, ...) {
  l <- list(...)
  l$path <- path
  out <- paste(capture.output(str(l)), collapse="\n<br>")

  list(payload=out,
       "content-type" = "text/html",
       "headers" = NULL,
       "status code"=200L
       )
}

##' Source file then make page
##'
##' @param path a file or sure. (Passed to source)
##' @param mimeType to specify mime type
processSource <- function(path, mimeType=mime_type(path)) {
  e <- new.env()
  results <- try(capture.output(do.call("source", list(path, local=TRUE), envir=e)), silent=TRUE)

  ## check that it worked
  if(inherits(results, "try-error")) 
    return(makeErrorPage(results))
                         
  ## OK
  ## scan through e looking for gWindow object
  objs <- getBaseObjectsFromEnvironment(e)

  w <- get(objs[1], envir=e)
  ID <- w$sessionID
  ## assign by session ID to some list
  sessionEnv <- get("..gWidgets_sessionEnv",  envir=.GlobalEnv)
  sessionEnv[[ID]] <- w
  assign("..gWidgets_sessionEnv", sessionEnv, envir=.GlobalEnv)
  
  ##    results <- capture.output(source(path))
  results <- paste(results, collapse="\n")
  out <- gWidgetsWWW:::makegWidgetsWWWpage(results, script=TRUE, .=w)
  ret <- list(payload=out,
              "content-type" = mimeType,
              "headers" = NULL,
              "status code"=200L
              )

}

##' run a gWidgetsWWW script
##' 
##' @param path path to file
##' @param query query string (path?var1=x&var2=u) -> path=path; query=c(var1="x", var2="u")
##' @param ... ignored
processRun <- function(path, query, ...) {
  
  path <- paste(path, collapse=.Platform$file.sep)


  if(!file.exists(path))
    return(makeErrorPage(sprintf("Can't find %s", path)))

  ## if mime_type is text/plain, otherwise we pass through
  if(mime_type(path) == "text/plain") {
    ret <- processSource(path, "text/html")
  } else {
    ret <- list(file=path,
                "content-type"=mime_type(path),
                "status code" = 200L)
  }
  return(ret)
}

##' run an external file (from a url)
##' query passes in url: /custom/gw/gWidgetsWWWRunExternal?url=http://www.math.csi.cuny.edu/test
processExternalRun <- function(path, query, ...) {
  path <- ourURLdecode(query[1])
  ret <- processSource(path, "text/html")
  return(ret)
}

##' process an AJAX call
##'
##' @param path ignored
##' @param query ignored
##' @param ... Passes in detail abou tPOST event
##' @details These all come as POST requests. This information is
##' passed through ..., not query. We call it query below, nonetheless
processAJAX <- function(path, query, ...) {
  query <- list(...)[[1]]               # query passed in body, not query (POST info)

  ## rstudio passes query as an object with a attr "application/x-www-form-urlencoded; charset=UTF-8"
  if(is.raw(query)) {
    out <- rawToChar(query)
    tmp <- unlist(strsplit(out, "&"))
    l <- list()
    for(i in tmp) {
      i <- ourURLdecode(i)
      a <- strsplit(i, "=")[[1]]
      if(length(a) > 1 && !is.na(a[2]))
        l[[a[1]]] <- a[2]
    }

    query <- l
  }

  query <- lapply(query, function(i) i) # make a list
  type <- query$type
  
  switch(type,
         "runHandler"= {
           l <- gWidgetsWWW:::localRunHandler(query$id, query$context, query$sessionID)
           ret <- list(payload=l$out,
                       "content-type"="text/javascript",
                       "headers"=NULL,
                       "status code"=l$retval
                       )
         },
         "assign" = {
           ## pass back return value. Assign does nothing otherwise
           l <- gWidgetsWWW:::localAssignValue(query$variable, ourURLdecode(query$value), query$sessionID)
           ret <- list(payload=l$out,
                       "content-type"="text/html",
                       "headers"=paste(
                         "<?xml version='1.0' encoding='ISO-8859-1'?>",
                         "<responseText></responseText>",
                         "<readyState>4</readyState>",
                         sep="\n"),
                       "status code"=l$retval
                       )
         },
         "clearSession"={
           ## clear out session
           clearSessionId(query$sessionID)
            ret <- list(payload="",
                       "content-type"="text/html",
                       "headers"=paste(
                         "<?xml version='1.0' encoding='ISO-8859-1'?>",
                         "<responseText></responseText>",
                         "<readyState>4</readyState>",
                         sep="\n"),
                       "status code"=200L
                       )
         },
         "fileupload"={
           ret <- makeErrorPage(sprintf("Don't know how to process type %s.", type))
         })
  ##  assign("ret", ret, envir=.GlobalEnv)
  return(ret)
}


##' basic handler to arrange for dispatch based on URL
##'
##' @param path passes in path including custom/gw bit
##' @param query passes in GET info
##' @param ... passes in post information (for AJAX calls!)
gw.httpd.handler <- function(path, query, ...) {

  ## here path is path, query contains query string, ... ???
  ## assign("path", path, envir=.GlobalEnv)
  ## assign("query", query, envir=.GlobalEnv)
  ## assign("post", list(...)[[1]], envir=.GlobalEnv)
  path <- ourURLdecode(path)
  query <- ourURLdecode(query)
  
  ## strip off /custom/url_base/
  path <- gsub(sprintf("^/custom/%s/",url_base), "", path)
  path <- unlist(strsplit(path, "/"))

  ## Dispatch on value of path[1]
  out <- switch(path[1],
                "ext"=processBasehtmlFile(path, query, ...),
                "images"=processBasehtmlFile(path, query, ...),
                "gWidgetsWWWRun"=processRun(path[-1], query, ...),
                "gWidgetsWWW" = processAJAX(path[-1], query, ...),
                "gWidgetsWWWRunExternal"=processExternalRun(path[-1], query, ...),
                processBasehtmlFile(c("",path), query,  ...)
                )
  ## assign("out", out, envir=.GlobalEnv)
  return(out)
}



##' is the server running
##'
isServerRunning <- function() {
  tools:::httpdPort > 0L
}

## for testing from outside package
url_base <- NULL
gWidgetsWWWAJAXurl <- NULL
gWidgetsWWWimageUrl <- NULL
gWidgetsWWWStaticDir <- NULL
.gWidgetsWWWisLocal <- NULL

##' start the local server for gWidgetsWWW
##'
##' @param file optional name of file to open
##' @param port ignored now
##' @param package If file specified and package given, the file looked up within package through system.file
##' 
##' @details Starts help server if not already done, then loads custom http handler
localServerStart <- function(file="", port=8079, package=NULL, ...) {
  if(!isServerRunning()) {
    tools:::startDynamicHelp()
  }

  if(!isServerRunning()) {
    ## XXX error didn't start
    gettext("XXX Server won't start")
    return()
  }

  ## store handler to respond to /custom/gw url
  if( exists( ".httpd.handlers.env", tools <- asNamespace("tools") ) ){
    e <- get( ".httpd.handlers.env", tools )
    e[["gw"]] <- gw.httpd.handler
  } else {
    gettext("XXX Odd, environment of httpd handlers is absent")    
    return()
  }

  ## configure some package-local variables
  assignInNamespace("url_base", "gw", ns="gWidgetsWWW")
  assignInNamespace("gWidgetsWWWAJAXurl",sprintf("/custom/%s/gWidgetsWWW", url_base), ns="gWidgetsWWW")
  assignInNamespace("gWidgetsWWWimageUrl", sprintf("/custom/%s/images/", url_base), ns="gWidgetsWWW")
  assignInNamespace(".gWidgetsWWWisLocal", TRUE, ns="gWidgetsWWW")

  ## global variables
  assign("gWidgetsWWWStaticDir", (tmp <- tempdir()), envir=.GlobalEnv)
  assign("gWidgetsWWWStaticUrlBase", sprintf("/custom/%s/gWidgetsWWWRun/%s", url_base, tmp), envir=.GlobalEnv)
  
  ## open if called to
  if(!is.null(file) && file != "") {
    localServerOpen(file, package, ...)
  } else {
    if(!is.null(file)) {
      ## make a message
      msg <- gettext(paste("You may create a web page from a file in your working directory via:",
                           "",
                           "     localServerOpen('filename')",
                           "",
                           "Otherwise, you can load a file from a package via:",
                           "",                           
                           "      localServerOpen('filename', package='pkgName')",
                           "",                           
                           "For example, to see a simple GUI try this:",
                           "",                           
                           "      localServerOpen('Examples/ex-simple-gui.R', package='gWidgetsWWW')",
                           "\n\n",
                           sep="\n"))
      cat(msg)
    }
  }
}


##' Load a file in gWidgets by calling gWidgetsWWWRun
##' @param file filename to open
##' @note  XXX Unix only? Test this
gWloadFile <- function(file, ...) {
  localServerStart(file=NULL)
  .url <- sprintf("http://127.0.0.1:%s/custom/%s/gWidgetsWWWRun/%s",
                  tools:::httpdPort,
                  url_base,
                  ourURLencode(file))
  browseURL(.url)
}

##' Load file from pacakge
##' @param file passed to system.file. If NULL, ignored
##' @param package to look for file
localServerOpen <- function(file, package=NULL, ...) {
  if(is.null(file))
    return()                            # do nothing
  if(!is.null(package))
    file <- system.file(file, package=package)
  if(file.exists(file))
    gWloadFile(file, ...)
  else
    cat(sprintf("Can't find file %s\n", file))
}

##' Source a file or url to write the web page
##'
##' @param file_or_url A file or url passed to source to produce the gWidgetsWWW web page
##' @return Opens a browser page
localServerSource <- function(file_or_url) {
  localServerStart(file=NULL)
  
  .url <- sprintf("http://127.0.0.1:%s/custom/%s/gWidgetsWWWRunExternal?url=%s",
                  tools:::httpdPort,
                  url_base,
                  ourURLencode(file_or_url))
  browseURL(.url)
}
  
gWidgetsWWWIsLocal <- function() {
  !is.null(getFromNamespace(".gWidgetsWWWisLocal", ns="gWidgetsWWW"))
}


##' Called by AJAX script to assign a value
localAssignValue <- function(id, value, sessionID) {
  e <- getBaseObjectFromSessionID(sessionID)
  OK <- 200L; ERROR <- 419L
  l <- list(out="", retval=OK)
  if(is.null(e)) {
    l$out <- sprintf("Error: can't find session for", sessionID, "\n")
    l$retval <- ERROR
  } else {
    out <- ourFromJSON(value)
    if(is.list(out)) {
      tmp <- try(assign(id, out$value, envir=e), silent=TRUE)
      if(inherits(tmp, "try-error")) {
        l$out <- tmp
        l$retval <- ERROR
      }
    }
  }
  return(l)
}

##' Called to run a handler
localRunHandler <- function(id, context=NULL, sessionID) {
  ## assign("id",id, envir=.GlobalEnv)
  ## assign("context", context, envir=.GlobalEnv)
  ## assign("sessionID", sessionID, envi=.GlobalEnv)

  if(!is.null(context)) {
    context <- ourURLdecode(context)
    if(context == "\"\"")
      context <- NULL
  }

  ## return 200 if ok, 419 if no
  OK <- 200L; ERROR <- 419L
  ret <- list(out="", retval=OK)
  
  e <- getBaseObjectFromSessionID(sessionID)
  if(is.null(e)) {
    ret$out <- "alert('No session for this id');"
    ret$retval <- ERROR
  } else if(sink.number() > 10) {
    ret$out <- sprintf("alert('too many sinks: %s');", sink.number())
    ret$retval <- ERROR
  } else {
    f <- tempfile()
    ## XXX There is an issue with SIGPIPE presumably related to the way we open a file to capture the output
    ## of the handler call. The handlers return javascript catted out. so we need to get it some way.
    ## This attempt to tey and open is not so great -- doesn't work.
    ctr <- 1
    tmp <- try(sink(f), silent=TRUE)
    while(inherits(tmp, "try-error") && ctr < 20) {
      ctr <- ctr + 1
      tmp <- try(sink(f), silent=TRUE)
    }
    if(ctr >= 20)
      stop("Tried too many times")
    
    out <- try({
      if(!is.null(context))
        e$runHandler(id, ourFromJSON(context))
      else
        e$runHandler(id)
    }, silent=TRUE)
    
    if(inherits(out, "try-error")) {
      sink(NULL)
      ret$out <- sprintf("Error: %s", as.character(out))
      ret$retval <- ERROR
    } else {
      sink(NULL)
      cat("\n", file=f, append=TRUE)
      ret$out <- paste(readLines(f), collapse="\n")
    }
    
    unlink(f)
  }
  return(ret)
}

## out <- tryCatch({
## tc <- textConnection("textfromconnection", open="w")
## sink(file=tc)
## if(nchar(context))
##   e$runHandler(id, ourFromJSON(context))
## else
##   e$runHandler(id)
## sink()
## close(tc)
      ## paste(textfromconnection,sep="",collapse="")
## }, error=function(e) {
    ##   sink()
##   close(tc)
##   cat('ERROR1: ')
##   cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
##   ""
## }, finally= {})
#}
#}


## find file and run from package
## This one requires us to put in headers. This allows
## files other than .R files to be served
mimeTypes <- function(ext) {
  switch(ext,
         "R","text/javascript",
         "txt"="text/plain",
         "htm"="text/html",
         "html"="text/html",
         "gif"="image/gif",
         "jpg"="image/jpeg",
         "png"="image/png",
         "svg"="image/svg+xml",
         "xbm"="image/x-xbitmap",
         "css"="text/css",
         "js "="application/x-javascript",
         "htc"="text/x-component",
         "xml"="text/xml",
         "pdf"="application/pdf",
         "eps"="application/postscript",
         "ps"="application/postscript",
         "text/html"
         )
}

makegWidgetsWWWPageHeader <- function(.) {
  ## XXX This needs work!! The proper combination here could make things work for Chrome, safari, Opera and IE?
  out <- paste(
#               "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",
#               "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>",
#               '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">',
#               "<html xmlns='http://www.w3.org/1999/xhtml' xmlns:v='urn:schemas-microsoft-com:vml'>",
               "<html xmlns:v=urn:schemas-microsoft-com:vml>",
               "<head>",
               "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />",
               "<!-- Call in Ext style sheet -->",
               "<link rel='stylesheet' type='text/css' href='/custom/gw/ext/resources/css/ext-all.css' />",
               "</head>",
               "<body>",
               "<!-- Call in Ext files -->",               
               "<div id='loading'>",
               "<div class='loading-indicator'>",
               "<img src='/custom/gw/images/extanim32.gif' width='32' height='32' style='margin-right:8px;float:left;vertical-align:top;'/>",
               "gWidgetsWWW",
               "<br />",
               "<span id='loading-msg'>Loading styles and images...</span>",
               "</div>",
               "<span id='loading-msg'></span>",
               "</div>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Core API...';</script>",
               "<script type='text/javascript' src='/custom/gw/ext/adapter/ext/ext-base.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading UI Components...';</script>",

               "<script type='text/javascript' src='/custom/gw/ext/ext-all.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",
               ## conditional includes -- values set in constructor on toplevel
               "<script type='text/javascript' src='/custom/gw/gWidgetsWWW.js'></script>",
               ## google stuff -- move out
               if(exists("ggooglemaps_key", .) && exists("do_googlemaps", .)) {
                 paste(
                       ## sprintf('<script type=\'text/javascript\' src=http://www.google.com/jsapi?key=%s></script>',.$ggooglemaps_key),
                       ## '<script type="text/javascript">  google.load("maps", "2"); </script>',
                       "<script type='text/javascript' src='/custom/gw/ggooglemaps/ext.ux.gmappanel.js'></script>" ,
                       '<meta name="viewport" content="initial-scale=1.0, user-scalable=no" />',
                       '<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>',
                       sep="\n")
               },
               ## end google
               ## webvis stuff move out
               if(exists("do_gwebvis", envir=.)) {
                 "<script type='text/javascript' src='/custom/gw/protovis/protovis-d3.1.js'></script>"
               },
               ##
               "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>",
               sep="\n")
  return(out)
}

makegWidgetsWWWpage <- function(results, script=TRUE, .=new.env()) {
  out <- makegWidgetsWWWPageHeader(.)
  out <-  paste(out,
               if(script) {
                 "<script type='text/javascript'>"
               },
               results,
               if(script) {
                 "</script>"
               },
               "<script type='text/javascript'>Ext.EventManager.on(Ext.getBody() , 'unload', clearSession)</script>",
               "</body>",
               "</html>",
               sep="\n")
  return(out)
}

##################################################
## These are the exported files

##' start local server.
##' @param file file to open with. If file not null, then calls makeIndex on current directory
##' @param port port to open. May conflict with help server,
##' @package If file and package  not given, opens default. Otherwise, file and package combined through localServerOpen
##' @export
## localServerStart <- function(file="", port=8079, package=NULL) {
##   startRpadServer("index.gWWW", port)   # just to keep it quiet
##   if(file == "" && is.null(package)) {
##     file <- "basehtml/makeIndex.R"
##     package <- "gWidgetsWWW"
##   }
##   localServerOpen(file, package)
## }

##' stop local server
localServerStop <- stopRpadServer <- function() .Deprecated("",msg="No longer needed")

##' restart local server
localServerRestart <- restartRpadServer <- function() .Deprecated("",msg="No longer needed")

##' open file wihin an optional package
##'
##' @param file name of file If package is null, relative to current directory
##' @param package optional package to look file up in.
##' @export
## localServerOpen <- function(file, package=NULL) {
##   ## open file
##   ## if package, then open from package found through
##   ## system.file(file, package=package)
##   ## if file matches R$, then use gWidgetsWWWrun
##   ## else pass through
##   if(!missing(package) && !is.null(package))
##     file <- sprintf("/custom/gw/gWidgetsWWWRunFromPackage/%s?package=%s",file, package)
  
##   if(length(grep("[rR]$", file)))
##     file <- sprintf("/custom/gw/gWidgetsWWWrun/%s", file)

##   port <- get("RpadPort", envir = .RpadEnv)
##   browseURL(sprintf("http://127.0.0.1:%s/%s", port, file))
## }

##' function to test if using local server
##' @export
## gWidgetsWWWIsLocal <- function() {
##   TRUE ## XXX fix me
##   exists(".RpadEnv", envir=.GlobalEnv) && get("RpadLocal", envir=.RpadEnv)
## }


##################################################
##
## Stuff left over from Rpad days.
## Not needed with merge to using dynamic help server

## ## The Tcl variable RpadTclResults is set to pass information back into the
## ## web server
## ## This is supposed to signal an error, but doesn't seem to be correct.
## RpadAssignValue <- function(id, value, sessionID) {
##   e <- getBaseObjectFromSessionID(sessionID)
##   retval <- "419"                         # expectation failed
##   if(is.null(e)) {
##     cat("Error: can't find session for", sessionID, "\n")
##   } else {
##     out <- ourFromJSON(value)
##     if(is.list(out)) {
##       tmp <- try(assign(id, out$value, envir=e), silent=TRUE)
##       if(!inherits(tmp, "try-error"))
##         retval <- "200"                   # all good
##     }
##   }
##   .Tcl(paste("set RpadTclResults {", escapeBrackets(retval), "}", sep=""))
##   return("")
## }


## RpadRunHandler <- function(id, context="", sessionID) {
##   e <- getBaseObjectFromSessionID(sessionID)
##   if(is.null(e)) {
##     .Tcl(paste("set RpadTclResults {alert('session has expired');}", sep="")) # return "" if fails
##   } else {
## results <- tryCatch({
##       tc <- textConnection("textfromconnection", open="w")
##       sink(file=tc)
##       if(nchar(context))
##         e$runHandler(id, ourFromJSON(context))
##       else
##         e$runHandler(id)
##       sink()
##       close(tc)
##       results <- paste(textfromconnection,sep="",collapse="")
## #      results <- gsub("\n","\\n",results)
##       .Tcl(paste("set RpadTclResults {", escapeBrackets(results), "}", sep=""))
##     }, error=function(e) {
##       sink()
##       close(tc)
##       cat('ERROR1: ')
##       cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
##       .Tcl("set RpadTclResults {}") # return "" if fails
##     }, finally= {})
##   }
## }

## RpadSourceScript <- function(file) {
##   file <- gsub("\\.\\.","", file)
##   if(!length(grep("[rR]$",file)))
##     file <- paste(file, ".R", sep="")
##   if(file.exists(paste(getwd(),file, sep=.Platform$file.sep))) {
##     results <- tryCatch({
##       tc <- textConnection("textfromconnection",open="w")
##       sink(file=tc)
##       source(file)
##       sink()
##       close(tc)
##       formattedresults <- paste(textfromconnection,"\n",sep="",collapse="")
##       ## Now we add some stuff to formatted results
##       ## header
##       out <-  String() + makegWidgetsWWWPageHeader()
##       if(!is.null(getOption("gWidgetsWWWGoogleAPI"))) {
##         out <- out +
##           paste("<script type='text/javascript' src=http://maps.google.com/maps?file=api&v=2&key=",
##                 getOption("gWidgetsWWWGoogleAPI"),
##                 "&sensor=false></script>", sep="")
##       }
##       out <- out + "<script type='text/javascript'>" +
##         formattedresults +
##           "</script>"
##       options("gWidgetsWWWGoogleAPI"=NULL) # must set in each script
##       .Tcl(paste("set RpadTclResults {", escapeBrackets(out), "}", sep=""))
##     }, error=function(e) {
##       sink()
##       close(tc)
##       cat('ERROR1: ')
##       cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
##       .Tcl(paste("set RpadTclResults {}", sep="")) # return "" if fails
##     }, finally= {})
##   } else {
##     ## can't find the file
##     cat("Error1: can't find file", file, "in directory", getwd())
##     .Tcl("set RpadTclResults {}")
##   }
## }


## RpadRunFromPackage <- function(file, package) {
##   filename <- system.file(file, package=package)
##   if(!file.exists(filename)) {
##     ## return error
##     out <- paste("HTTP/1.1 404 File Not Found","\n",
##                  "Date:", date(),
##                  "Connection: close" , "\n\n", sep="")
##   } else {
##     ## modify this, and put text into results
##     output <- list(header="HTTP/1.1 200 Data follows",
##                    contentType="text/html")
    
##     ## is it a directory?
##     if(file.info(filename)$isdir) {
##       files <- list.files(filename, pattern="R$")
      
##       results <- "Choose a file<br><UL>"
##       results <- paste(results,
##                        paste("<LI><A href=/custom/gw/gWidgetsWWWRunFromPackage/",
##                              paste(file,files,sep="/"),
##                              "?package=",package,
##                              ">",files,"</A></LI>",
##                              sep="", collapse=""),
##                        "</UL>", sep="")
##       results <- makegWidgetsWWWpage(results, script=FALSE)
##     } else {
##       ## it's a file
##       baseFile <- basename(filename)
##       ext <- rev(unlist(strsplit(baseFile,"\\.")))[1]
      
##       ## we need to treat some types of files differently
##       if(ext == "R") {
##         ## source, then output as javascript
##         results <- tryCatch({
##           tc <- textConnection("textfromconnection",open="w")
##           sink(file=tc)
##           source(filename)
##           sink()
##           close(tc)
##           formattedresults <- paste(textfromconnection,"\n",sep="",collapse="")
##           makegWidgetsWWWpage(formattedresults)
##         }, error=function(e) {
##           sink()
##           close(tc)
##           cat('ERROR1: ')
##           cat(paste(paste(textfromconnection, "\n", collapse=""), '\n', e),"\n")
##         }, finally= {})
##       } else {
##         ## just change contentType
##         output$contentType <- mimeTypes(ext)
##         results <- paste(readLines(filename), collapse="\n", warn=FALSE)
##       }
##     }
##     out <- paste(output$header,
##                  output$contentType,
##                  "",
##                  results,
##                  sep="\n")
    
    
##     ## set the variable then return
##     .Tcl(paste("set RpadTclResults {",
##                escapeBrackets(paste(out, collapse="\n"))
##                , "}", sep=""))
##     return()
##   }
## }
  

## "processRpadCommands" <-
## function() {
##   require("tcltk")
##   commands <- tclvalue(.Tcl("set user(R_commands)"))
##   textcommands <- textConnection(commands)

##   results <- tryCatch({
##     tc <- textConnection("textfromconnection",open="w")
##     sink(file=tc)
##     guiSource(textcommands)
##     sink()
##     close(tc)
##     textfromconnection
##   }, error=function(e) {
##     sink()
##     close(tc)
##     cat('ERROR1: ')
##     paste(paste(textfromconnection, "\n", collapse=""), '\n', e)},
##                       finally=close(textcommands))
##   formattedresults <- paste(results,"\n",sep="",collapse="")
##   .Tcl(paste("set RpadTclResults {", escapeBrackets(formattedresults), "}", sep=""))
## }

##################################################
## functions to start/stop the server

## "Rpad" <-
## function(file = "", defaultfile = "LocalDefault.Rpad", port = 8079) {
##     startRpadServer(defaultfile, port)
##     if(file=="")
##       file <- "/custom/gw/gWidgetsWWWRunFromPackage/basehtml/makeIndex.R?package=gWidgetsServer"
##     browseURL(paste("http://127.0.0.1:", port, file, sep = ""))
## }




## "startRpadServer" <-
## function(defaultfile = "index.gWWW", port = 8079) {
##     require("tcltk")
##     ## This is the main function that starts the server
##     ## This function implements a basic http server on 'port'
##     ## The server is written in Tcl.
##     ## This way it is not blocking the R command-line!

##     if (!require("tcltk")) stop("package tcltk required for the local Rpad http server")

##     ## Need to set some variables
##     ## we set this values, as they are used by the scripts
##     ## These files are under basehtml in the main directory

##     getOptionWithDefault <- function(x, default) {
##       x <- getOption(x)
##       if(is.null(x))
##         x <- default
##       return(x)
##     }
##     ## TODO(JV): Make this smarter, for now gWidgetsWWW server uses global variables
##     ## so this must too
##     assignOption <- function(x, default) {
##       a <- getOptionWithDefault(x, default)
##       assign(x, a, envir=.GlobalEnv)
##     }
##     assignOption("extjsBaseUrl",'/ext')
##     assignOption("gWidgetsWWWimageUrl",'/images/')
    
##     ## directory and baseurl for static html files, images, svg graphics, ...
##     ## This needs to be writeable by the web server process
##     ## May need to unlink files that accumulate here!
##     assignOption("gWidgetsWWWStaticDir", paste(getwd(),"/",sep=""))
##     assignOption("gWidgetsWWWStaticUrlBase","/")

##     ## set in key= prop of ggoglemaps
## #    options("gWidgetsWWWGoogleAPI"="ABQIAAAAYpRTbDoR3NFWvhN4JrY1ahS5eHnalTx_x--TpGz1e2ncErJceBS7FrNBqzV5DPxkpbheIzZ9nTJPsQ") ## key for 127.0.0.1:8079
##     options("gWidgetsWWWGoogleAPI"=NULL)
##     assignOption("gWidgetsWWWGoogleAPI","NULL")
##     assignOption("gWidgetsWWWRunGoogle","FALSE") # ## Set to TRUE to show google
    
##     ## Load in session code -- not needed for local server
##     assignOption("sessionSecretKey", "abcdefg")
    
##     ## gWidgets AJAX setup
##     ## this is needed to handle www <--> R interface
##     assignOption("gWidgetsWWWAJAXurl", "/gWidgetsWWWAJAX")
    

##     ## Rpad uses environment to keep values, We place in global environment
##     e <- new.env()
##     assign(".RpadEnv", e, envir=.GlobalEnv)
##     assign("RpadLocal", TRUE, envir = e)
##     assign("RpadDir",   ".",  envir = e)
##     assign("RpadPort",  port, envir = e)

##     tclfile <- system.file( "tcl", "mini1.1.tcl", package = "gWidgetsWWW")
##     htmlroot <- system.file("basehtml",package = "gWidgetsWWW")
##     tcl("source", tclfile)
##     tcl("Httpd_Server", htmlroot, port, defaultfile)
##     return(TRUE)
## }

## "stopRpadServer" <-
## function() {
##     require("tcltk")
##     e <- .RpadEnv
##     assign("RpadLocal", FALSE, envir = e)
##     assign("RpadDir",   NULL,  envir = e)
##     assign("RpadPort",  NULL, envir = e)

##     .Tcl("close $Httpd(listen)")
##     .Tcl("unset Httpd")
## }

## "restartRpadServer" <-
## function() {
##   stopRpadServer()
##   startRpadServer()
## }

