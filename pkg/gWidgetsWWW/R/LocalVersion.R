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



##' Environment to hold different sessions
assign("..gWidgets_sessionEnv", new.env(), envir=.GlobalEnv)

##' remove session from list to free up memory
clearSessionId <- function(ID) {
  sessionEnv <- get("..gWidgets_sessionEnv",  envir=.GlobalEnv)
  sessionEnv[[ID]] <- NULL
  assign("..gWidgets_sessionEnv", sessionEnv, envir=.GlobalEnv)
}

##' This lists the gwindow objects and matches against ID
getBaseObjectFromSessionID <- function(sessionID, envir=.GlobalEnv) {
  sessionEnv <- get("..gWidgets_sessionEnv",  envir=.GlobalEnv)  
  return(sessionEnv[[sessionID]])
}

##' return all gWindow instances in an environment
##'
##' @param environment to search in
getBaseObjectsFromEnvironment <- function(envir=.GlobalEnv) {
  vars <- ls(envir=envir)
  ind <- sapply(vars, function(i) inherits(get(i, envir=envir), "gWindow"))
  if(any(ind))
    gWindowObjects <- vars[ ind ]
  else
    gWindowObjects <- character(0)
  return(gWindowObjects)
}
  
##' escape brackets in a string
##'
##' @param x character value to escape
##' @return character
escapeBrackets <- function(x) gsub("(\\{|\\})", "\\\\\\1", x)


##' From dynamicHelp in tools:
##'
##' Find mime type from file extension
##' (Taken from Duncan Murdoch's work)
##' @param path file name
##' @return mime type
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
##' 
##' @param msg String. error message
##' @param status_code Integer. use to specify status code. Default if 400L
##' @return Returns a list with components indicating the payload (the
##' text to display), the content type and the status code. 
makeErrorPage <- function(msg, status_code=400L) {
  list(payload=paste(msg, collase="\n"),
       "content-type"="text/html",
       "status code"=status_code)
}

##' Process a script through source and capture ouutput
##'
##' @param file filename of script
##' @param content_type MIME type of output
##' @return list containng output and status
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
##' @return payload to send to web server
processBasehtmlFile <- function(path, query, ...) {
  f <- sprintf("/basehtml/%s", strip_slashes(paste(path, collapse="/")))
  f <- gsub("//","/", f)                # just in case
  f <- system.file(f, package="gWidgetsWWW")

  if(!file.exists(f))
    return(makeErrorPage(sprintf("Can't find %s", f)))

  ## see https://code.google.com/speed/page-speed/docs/caching.html
  ## doesn't seem to work with firefox
  temp <- "%a %b %e %Y %H:%M:%S GMT%z (%Z)"

  cacheControl <- "Cache-Control: max-age=31536000"
  expires <- sprintf("Expires: %s", format(Sys.time() + 60*60*24, temp))
  lastModified <- sprintf("Last-Modified: %s", format(file.info(f)$mtime, temp))

  list(file=f, 
       "content-type" = mime_type(f),
       "headers" = c(cacheControl, expires, lastModified),
       "status code"=200L
       )

}

##' process file from static directory
##' @param path path to file
##' @param query query string (path?var1=x&var2=u) -> path=path; query=c(var1="x", var2="u")
##' @param ... ignored
##' @return payload to return to web server
processStaticFile <- function(path, query, ...) {
  f <- sprintf("%s%s",.Platform$file.sep, Reduce(file.path, path))
  if(!file.exists(f))
    return(makeErrorPage(sprintf("Can't find %s", f)))

  ## see https://code.google.com/speed/page-speed/docs/caching.html
  ## doesn't seem to work with firefox
  temp <- "%a %b %e %Y %H:%M:%S GMT%z (%Z)"

  cacheControl <- "Cache-Control: max-age=31536000"
  expires <- sprintf("Expires: %s", format(Sys.time() + 60*60*24, temp))
  lastModified <- sprintf("Last-Modified: %s", format(file.info(f)$mtime, temp))

  list(file=f, 
       "content-type" = mime_type(f),
       "headers" = c(cacheControl, expires, lastModified),
       "status code"=200L
       )

}

##' Stub to inspect path, query, ... values
##' @param path path to file
##' @param query query string (path?var1=x&var2=u) -> path=path; query=c(var1="x", var2="u")
##' @param ... ignored
##' @return payload to send to web server
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
##' @return payload to send to web server
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
##' @return payload to send to web server
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
##' 
##' query passes in url: /custom/gw/gWidgetsWWWRunExternal?url=http://www.math.csi.cuny.edu/test
##' @param path ignored
##' @param query the path is the first entry.
##' @return calls processSource
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
##' @return a payload to send to web server
processAJAX <- function(path, query, ...) {
  if(is.null(query))
    query <- list(...)[[1]]               # query passed in body, not query (POST info, not GET)

  
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

  if(is.null(type))
    type <- path[1]                     # for calling in url

  switch(type,
         "runHandler"= {
           l <- gWidgetsWWW:::localRunHandler(query$id, query$context, query$sessionID)
           ret <- list("payload"=l$out,
                       "content-type"="application/javascript",
                       "headers"=NULL,
                       "status code"=l$retval
                       )
           return(ret)
         },
         "assign" = {
           ## pass back return value. Assign does nothing otherwise
           l <- gWidgetsWWW:::localAssignValue(query$variable, ourURLdecode(query$value), query$sessionID)
           ret <- list(payload=l$out,
#                       "content-type"="text/html",
                        "content-type"="application/javascript",
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
           ## For some reason this setup gives a SIGPIPE error and it isn't in the call to clearSessionID
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
         "proxystore"={
           ## localProxyStore dispatches to a method of the store which considers query
           ## path[2] is the id of the widget, path[3] is the sessionID. These are passed
           ## in via the URL -- not the query
           l <- gWidgetsWWW:::localProxyStore(path[2], path[3], query)
           ret <- list(payload=l$out,
                       "content-type"="application/json",
                       "headers"=NULL,
                       "status code"=200L
                       )
         },
         "fileupload"={
           ret <- makeErrorPage(sprintf("Don't know how to process type %s.", type))
         })
  return(ret)
}


##' basic handler to arrange for dispatch based on URL
##'
##' @param path passes in path including custom/gw bit
##' @param query passes in GET info
##' @param ... passes in post information (for AJAX calls!)
##' @return output of functions is a payload list.
gw.httpd.handler <- function(path, query, ...) {


  ## here path is path, query contains query string, ... ???
  path <- ourURLdecode(path)
  query <- ourURLdecode(query)

  
  ## strip off /custom/url_base/
  path <- gsub(sprintf("^/custom/%s/",url_base), "", path)
  ## strip any trailing slash
  path <- gsub("[/]*$", "", path)
  path <- unlist(strsplit(path, "/"))


  ## Dispatch on value of path[1]
  out <- switch(path[1],
                "ext"=processBasehtmlFile(path, query, ...),
                "images"=processBasehtmlFile(path, query, ...),
                "static"=processStaticFile(path[-1], query, ...),
                "gWidgetsWWWRun"=processRun(path[-1], query, ...),
                "gWidgetsWWW" = processAJAX(path[-1], query, ...),
                "gWidgetsWWWRunExternal"=processExternalRun(path[-1], query, ...),
                processBasehtmlFile(c("",path), query,  ...)
                )
  return(out)
}



##' is the server running
##'
##' @return logical
isServerRunning <- function() {
  tools:::httpdPort > 0L
}

## for testing from outside package
##' global value for base url
url_base <- NULL
##' global value for AJAX url
gWidgetsWWWAJAXurl <- NULL

##' global for image directory url
gWidgetsWWWimageUrl <- NULL

##' global for directory where static files are to go
gWidgetsWWWStaticDir <- NULL

##' global variable to indicate if running locally or via rapache server
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
  assign("gWidgetsWWWStaticUrlBase", sprintf("/custom/%s/static/%s", url_base, tmp), envir=.GlobalEnv)
  
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
##' @return NULL
gWloadFile <- function(file, ...) {
  localServerStart(file=NULL)
  .url <- sprintf("http://127.0.0.1:%s/custom/%s/gWidgetsWWWRun/%s",
                  tools:::httpdPort,
                  url_base,
                  strip_slashes(ourURLencode(file), leading=FALSE))
  browseURL(.url)
}

##' Load from a URL
##'
##' First downloads to a temporary file, then loads that
##' @param file a url for the script
gWloadFromURL <- function(file, ...) {
  tmp <- tempfile()
  out <- download.file(file, tmp)
  if(out == 0L) {
    localServerStart(file=NULL)
    .url <- sprintf("http://127.0.0.1:%s/custom/%s/gWidgetsWWWRun/%s",
                    tools:::httpdPort,
                    url_base,
                    tmp)
    browseURL(.url)
  } else {
    cat(sprintf("Error downloading %s.", file))
  }
}

##' Load file from pacakge
##' @param file, url or connection. File is full file path or if package is
##' non-NULL found from system.file(file, package=package). If a
##' connection, a temporary file is found to use and the connection is
##' closed.
##' @param package to look for file
##' @return NULL (opens page or gives message)
##' @note at this point, we should have written this to dispatch on the type of file.
localServerOpen <- function(file, package=NULL, ...) {
  if(missing(file))
    return()
  
  if(is(file, "connection")) {
    f <- tempfile(".R")
    cat("", file=f)
    sapply(readLines(file, warn=FALSE), function(i) cat(i,"\n", file=f, append=TRUE))
    close(file)
    file <- f
  } else if(isURL(file)) {
    localServerSource(file)
  } else if(!is.null(package)) {
    file <- system.file(file, package=package)
  }
  if(file.exists(file))
    gWloadFile(file, ...)
  else
    cat(sprintf("Can't find file %s\n", file))
}

##' return values
##'
##' Return error code if TRUE, OK if FALSE
##' $param val logical TRUE if there was an error
##' @return integer the status code of error or not
wasError <- function(val) {
  ifelse(val, 419L, 200L)
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

##' Is gWidgetsWW running from the local server
##'
##' @return logical
gWidgetsWWWIsLocal <- function() {
  !is.null(getFromNamespace(".gWidgetsWWWisLocal", ns="gWidgetsWWW"))
}


##' Called by AJAX script to assign a value in the local session
##'
##' @return list with error code and message
##' @TODO should assign using svalue method. This just puts into local environment to be picked up later
localAssignValue <- function(id, value, sessionID) {
  e <- getBaseObjectFromSessionID(sessionID)
  l <- list(out="", retval=wasError(FALSE))
  if(is.null(e)) {
    l$out <- sprintf("Error: can't find session for", sessionID, "\n")
    l$retval <- wasError(TRUE)
  } else {
    out <- ourFromJSON(value)
    e$assignValue(id, out)
    
    if(is.list(out)) {
      tmp <- try(assign(id, out$value, envir=e), silent=TRUE)
      if(inherits(tmp, "try-error")) {
        l$out <- tmp
        l$retval <- wasError(TRUE)
      }
    }
  }
  return(l)
}

##' Called to run a handler
##'
##' @id widget id
##' @param context passed into give context
##' @sessionID sessionID used to find the environment
##' @return payload to send to web server
localRunHandler <- function(id, context=NULL, sessionID) {

  if(!is.null(context)) {
    context <- ourURLdecode(context)
    if(context == "\"\"")
      context <- NULL
  }

  ## return 200 if ok, 419 if no
  OK <- 200L; ERROR <- 419L
  ret <- list(out="", retval=OK)
  
  e <- getBaseObjectFromSessionID(sessionID)
  ## sanity checks
  if(is.null(e)) {
    ret$out <- "alert('No session for this id');"
    ret$retval <- wasError(TRUE)
  } else {
    ## runHandler calls methods which first send javascript to the queue
    ## then we run the queue
    if(!(is.null(context) || context == ""))
      ret$out <- try(e$runHandler(id, ourFromJSON(context)), silent=TRUE)
    else
      ret$out <- try(e$runHandler(id), silent=TRUE)
    if(inherits(ret$out, "try-error")) {
      ret$out <- sprintf("<br />Error: %s", paste(ret$out,collapse="<br />"))
      ret$retval <- wasError(TRUE)
    }
  }
  return(ret)
}

##' return data from proxy store
##'
##' Calls the stores parseQuery method 
##' @param id id of store
##' @param sessionID session id to look up store
##' @param query  Contains parameters passed by ext. Passed to parseQuery method of store
##' @return payload to send to server
localProxyStore <- function(id, sessionID, query) {
  e <- getBaseObjectFromSessionID(sessionID)
  store <- e$getStoreById(id)

  ## return 200 if ok, 419 if no
  OK <- 200L; ERROR <- 419L
  ret <- list(out="", retval=OK)
  
  ## sanity checks
  if(is.null(e)) {
    ret$out <- "alert('No session for this id');"
    ret$retval <- ERROR
  } else {
    ## set ret$out. If an error set ret$reval <- ERROR
    x <- paste(capture.output(query), collapse="\n")
    ret$out <- x
  }

  out <- try(store$parseQuery(query), silent=TRUE)
  ret <- list(out=out,
              retval=wasError(inherits(out, "try-error")))
  return(ret)
}
  

##' Make a page header for a locally served script
##'
##' @return html code for the page header
makegWidgetsWWWPageHeader <- function(.) {
  ## XXX This needs work!! The proper combination here could make things work for Chrome, safari, Opera and IE?
  out <- paste(
#               "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>",
#               "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>",
#               '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">',
#               "<html xmlns='http://www.w3.org/1999/xhtml' xmlns:v='urn:schemas-microsoft-com:vml'>",
#               "<html xmlns:v=urn:schemas-microsoft-com:vml>",
               "<html>",
               "<head>",
               "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />",
               "<!-- Call in Ext style sheet -->",
               "<link rel='stylesheet' type='text/css' href='/custom/gw/ext/resources/css/ext-all.css' />",
#               "<link rel='stylesheet' type='text/css' href='/custom/gw/ext/examples/ux/css/ux-all.css' />",
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
#               "<script type='text/javascript' src='/custom/gw/ext/adapter/ext/ext-base-debug.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading UI Components...';</script>",

               "<script type='text/javascript' src='/custom/gw/ext/ext-all.js'></script>",
#               "<script type='text/javascript' src='/custom/gw/ext/ext-all-debug-w-comments.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading extra libraries...';</script>",

               "<script type='text/javascript' src='/custom/gw/ext/examples/ux/ux-all.js'></script>",
               "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",
               
               ## conditional includes -- values set in constructor on toplevel
               "<script type='text/javascript' src='/custom/gw/gWidgetsWWW.js'></script>",
               ## ## google stuff -- move out
               ## if(exists("ggooglemaps_key", .) && exists("do_googlemaps", .)) {
               ##   paste(
               ##         ## sprintf('<script type=\'text/javascript\' src=http://www.google.com/jsapi?key=%s></script>',.$ggooglemaps_key),
               ##         ## '<script type="text/javascript">  google.load("maps", "2"); </script>',
               ##         "<script type='text/javascript' src='/custom/gw/ggooglemaps/ext.ux.gmappanel.js'></script>" ,
               ##         '<meta name="viewport" content="initial-scale=1.0, user-scalable=no" />',
               ##         '<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>',
               ##         sep="\n")
               ## },
               ## end google
               ## webvis stuff move out
               ## if(exists("do_gwebvis", envir=.)) {
               ##   "<script type='text/javascript' src='/custom/gw/protovis/protovis-d3.1.js'></script>"
               ## },
               ##
               "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>",
               sep="\n")
  return(out)
}

##' Make a web page for the results
##'
##' @param results results of source a script file
##' @param script logical If TRUE wrap in script tags
##' @param . Ignored. Might be usefule for conditionally making the page header
##' @return HTML+javasscript code to write out to the server
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
                ## XXX This gives issues with the canvas example. Not sure why
                ## Causes SIGPIPE ERROR. Not the clearSession call, this invocation?
                ## "<script type='text/javascript'>Ext.EventManager.on(Ext.getBody() , 'unload', clearSession)</script>",
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
##' @param package If file and package  not given, opens default. Otherwise, file and package combined through localServerOpen
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
##'
##' Deprecated
localServerStop <- stopRpadServer <- function() .Deprecated("",msg="No longer needed")

##' restart local server
##'
##' Deprecated
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


## Some "apps" from the apps directory

##' a package browser/installer
##'
##' Available for local installs only
##' @return makes a web page for managing installation/loading of packages
##' @export
gw_package <- function() {
  localServerOpen("apps/gw_package.R", package="gWidgetsWWW")
}

##' A simple workspace browser
##'
##' Available for local use
##' @return Creates a web page for browsing objects in the workspace
##' @export
gw_browseEnv <- function() {
  localServerOpen("apps/gw_browse.R", package="gWidgetsWWW")
}
  
