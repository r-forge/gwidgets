## This script handles the interface between a gWidgetsWWW GUI and R
## The GUI calls this as a form using an AJAX request
## This script returns javascript to be processed
## we limit the ability to call R from the browser to a) assignment to a gWidget
## variable b) to run a handler by its id c) a proxystore request d) fileupload?

## This script requires some packages and variables for keeping
## track of the session to be set

## the hack to find the lone gwindow could be improved so that we can
## have more than one per window. This would allow us to mix html and
## gWidgets.

require(rjson)
require(gWidgetsWWW, quietly=TRUE)

## For error reporting we change this flag to TRUE and set an error message
## Currently we don't know how to pass this error message back to RApache
.sendError <- FALSE
.sendErrorMessage <- ""
sendError <- function(db, msg) {
  .sendError <<- TRUE
  .sendErrorMessage <<- msg
}

## must figure out type. Type can be set in POST value -- or be buried in the path type/id/sessionID
if(is.null(POST) || is.null(POST$type)) {

  ## we use the uri and strip off the AJAXurl
#  path <- SERVER$path
#  path <- unlist(strsplit(path, "/"))

  opath <- path <- SERVER$uri
  path <- gsub(gWidgetsWWWAJAXurl,"", path)
  path <- gsub("^/","",path)
  path <- unlist(strsplit(path, "/"))

  
  type <- path[1]
  id <- path[2]
  sessionID <- path[3]
} else {
  opath <- path <- c()
  type <- POST$type
  id <- POST$id                         # if present
  sessionID <- POST$sessionID
}



if(!.sendError && !is.null(type)) {

#  RApacheOutputErrors(TRUE, 'alert("', '");')
  RApacheOutputErrors(FALSE, '', '')
  

  if(exists("sessionDBIlogfile"))
    cat(sessionID, "\n", file=sessionDBIlogfile, append=TRUE)
  
  ## db object
  createDb(sessionDbFile) ## if not already there

  db <- initDb(sessionDbFile, type=sessionDbType)

  if(exists("sessionDBIlogfile"))
    cat(sessionID, "load*ed* dbfile", "\n", file=sessionDBIlogfile, append=TRUE)

  if(inherits(db, "try-error")) {
    sendError(db, "Error opening data base file")
  } 
  
  ## ## tidy up -- delete old sessions
  ## if(!dbExists(db,"lastClearTime") ||
  ##    db[["lastClearTime"]] + 7 * 24 * 60 * 60 < Sys.time()) { # 7 days??
  ##   keys <- dbList(db)
  ##   if(length(keys > 1)) {
  ##     keys <- keys[keys != "lastClearTime"]
  ##     for(key in keys) {
  ##       session <- db[[key]]
  ##       if(sessionHasTimedout(session))
  ##         dbDelete(db,key)
  ##     }
  ##   }
  ##   db[['lastClearTime']] <- Sys.time()
  ## }
  
  if(exists("sessionDBIlogfile"))
    cat(sessionID, "check valid key", "\n", file=sessionDBIlogfile, append=TRUE)

  ## is valid id?
  if(!.sendError)
    out <- validKey(db, sessionID)

  if(!out$retval) {
    sendError(db, out$reason)
  }
  
  ## get environment from ID
  if(!.sendError)
    e <- db[[sessionID]]
  
  
  ## has session timed out
  if(!.sendError && sessionHasTimedout(e)) 
   sendError(db, "Session timed out")
  
  ## hack to find gWindow instance. Must be only one.
  if(!.sendError) {
    ## XXX Can do better here. We have the enviroment and the ID of the
    ## window so can search over these values and get IDs
    nms <- ls(e)
    ind <- sapply(nms, function(i) {
      is(get(i, envir=e), "gWindow")
    })
    if(any(ind)) {
      w <- nms[ind]
      if(length(w) > 1) {
        ind <- sapply(w, function(i) e[[i]]$sessionID == sessionID)
        w <- w[ind]
      }
    } else {
      sendError(db, "Can't find gwindow instance")
    }
  }
  
  ## Here we process the type of request
  ## For a given type we do:
  ## 1) set the content type
  ## 2) if no errro, cat out generated commands
  ## 3) If there is an error, we do not cat, but instead call sendError

  
  ## what type of request. Just a few
  if(!.sendError) {
    if(type == "runHandler") {
      ## Run a handler call. Returns javascript
      if(exists("sessionDBIlogfile"))
        cat(sessionID, "run handler", "\n", file=sessionDBIlogfile, append=TRUE)
      
      
      ## Here we run a handler callback
      ## the environment does not remember the loaded packages
      require(gWidgetsWWW, quietly=TRUE)
      
      ## may need to quiet down via sink
      if(is.null(POST$context)) {
        out <- try(e[[w]]$runHandler(POST$id), silent=TRUE)
      } else {
        if(exists("sessionDBIlogfile"))
          cat(sessionID, "POST context:", POST$context, "\n", file=sessionDBIlogfile, append=TRUE)
        
        context <- try(fromJSON(as.character(POST$context)), silent=TRUE)
        out <- try(e[[w]]$runHandler(POST$id, context), silent=TRUE)
      }
      
      if(exists("sessionDBIlogfile"))
        cat(sessionID, "ran handler", "\n", file=sessionDBIlogfile, append=TRUE)
      
      
      if(inherits(out, "try-error")) {
        sendError(db, out)
      } else {
        ## read out
        setContentType("text/plain")
        if(!inherits(out,"try-error"))
          cat(out)
        
      ## store session
        db[[sessionID]] <- e
      }
    } else if(type == "assign") {
      ## Assign a value. 
      ## Nothing to report for assign unless there is an error
      
      
      ## Assign a value in the gwindow object
      ## only names of  gWidgetsXXX are permitted
      
      variable <- POST$variable
      value <- POST$value
      value <- fromJSON(value)            # store as JSON object
      value <- value$value                # JSON object is a list(value=xxx)
      
      ## check that variable matches
      ## Could put in check to limit size of value, o/w the post
      ## data could be arbitrarily large
      if(length(grep("^gWidgetID", variable)) > 0) {
        ## had sink() call here, removed. Was it required?
#        tmp <- e[[w]]
#        out <- try(assign(variable, value, envir=tmp), silent=TRUE) ## not e[[w]] here.
#        e[[w]] <- tmp

        w <- e[[w]]
        out <- try(w$assignValue(variable, value), silent=TRUE)
        
        ## log errors
        if(inherits(out,"try-error")) {
          sendError(db, out)
        } else {
          ## nothing to report, just commit changes to environment
          db[[sessionID]] <- e
        }
      } else {
        sendError(db, sprintf("var name %s is not acceptable", variable))
      }
    } else if(type == "proxystore") {
      
      ## get info from proxy store
      w <- e[[w]]
      query <- POST
      
      store <- w$getStoreById(id)
      out <- try(store$parseQuery(query), silent=TRUE)
      
      if(!inherits(out,"try-error")) {
        setContentType("application/javascript")
        cat(out)
      } else {
        sendError(db, sprintf("Error: %s", out))
      }
    } else if(type == "fileupload") {
      ## XXX Implement me with security
      print(file.info(POST[["file-path"]])) # from name of form
      system(sprintf("mv %s /tmp/test.file", POST[['file-path']]))
      
      if(1) {
        ## JSON code for output if success
        out <- sprintf("{'success' : true, msg : { files : [ {'file' : 1 , 'fileStatus' : '0', 'fileName' : '%s' } ] } }",
                       "insert name")
        setContentType("application/json")
        cat(out)
      } else {
        cat(404L)                         # some error goes here
      }
    } else if(type == "clearSession") {
      ## untaint
      sessionID <- gsub("[^a-zA-Z0-9]","",sessionID)
      f <- paste(sessionDbFile,sessionID, sep=.Platform$file.sep)
      if(file.exists(f))
        unlink(f)
      if(exists("sessionDBIlogfile"))
        cat(sessionID, "unlink ",f, "\n", file=sessionDBIlogfile, append=TRUE)
    }
    
    ## why don't we disconnect?
    ##  out <- try(dbDisconnect(db), silent=TRUE)
  }
}

## Question: how to return the error message to browser. I get stuck
## with general 500 warning
if(.sendError) {
  try(cat(paste(capture.output(.sendErrorMessage), collapse="\n"), "\n", file="/tmp/error.txt"), silent=TRUE)
}
if(.sendError) {
  HTTP_INTERNAL_SERVER_ERROR
} else {
  DONE
}
