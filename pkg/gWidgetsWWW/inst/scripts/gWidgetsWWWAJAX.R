## This script handles the interface between a gWidgetsWWW GUI and R
## The GUI calls this as a form using an AJAX request
## This script returns javascript to be processed
## we limit the ability to call R from the browser to a) assignment to a gWidget
## variable or b) to run ahandler by its id.

## This script requires some packages and variables for keeping
## track of the session to be set

require(rjson)

sendError <- function(db, msg) {
#  out <- try(dbDisconnect(db), silent=TRUE)
  stop(paste("alert('", msg, "');"))
}


if(!is.null(POST) && !is.null(POST$type)) {

  RApacheOutputErrors(TRUE, '', '')

  
  type <- POST$type
  ## get session ID    
  sessionID <- POST$sessionID


  if(exists("sessionDBIlogfile"))
    cat(sessionID, "\n", file=sessionDBIlogfile, append=TRUE)


  
  ## db object
  createDb(sessionDbFile) ## if not already there

  if(exists("sessionDBIlogfile"))
    cat(sessionID, "load dbfile", "\n", file=sessionDBIlogfile, append=TRUE)

  
  db <- initDb(sessionDbFile, type=sessionDbType)

  if(exists("sessionDBIlogfile"))
    cat(sessionID, "load*ed* dbfile", "\n", file=sessionDBIlogfile, append=TRUE)

  if(inherits(db, "try-error")) {
    sendError(db, "Error opening data base file")
  } 
  
  ## tidy up -- delete old sessions
  if(!dbExists(db,"lastClearTime") ||
     db[["lastClearTime"]] + 7 * 24 * 60 * 60 < Sys.time()) { # 7 days??
    keys <- dbList(db)
    if(length(keys > 1)) {
      keys <- keys[keys != "lastClearTime"]
      for(key in keys) {
        session <- db[[key]]
        if(sessionHasTimedout(session))
          dbDelete(db,key)
      }
    }
    db[['lastClearTime']] <- Sys.time()
  }
  
  if(exists("sessionDBIlogfile"))
    cat(sessionID, "check valid key", "\n", file=sessionDBIlogfile, append=TRUE)

  ## is valid id?
  out <- validKey(db, sessionID)
  if(!out$retval) {
    sendError(db, out$reason)
  }
  
  ## get environment from ID
  e <- db[[sessionID]]
  
  
  ## has session timed out
 if(sessionHasTimedout(e)) {
#   dbClear(db, sessionID)
   sendError(db, "Session timed out")
 }

  ## hack to find gWindow instance. Must be only one.
  nms <- ls(e)
  w <- nms[sapply(nms, function(i) class(get(i, envir=e))[1]) == "gWindow"]
  
  ## what type of request. Just a few
  if(type == "runHandler") {
    if(exists("sessionDBIlogfile"))
      cat(sessionID, "run handler", "\n", file=sessionDBIlogfile, append=TRUE)


    ## Here we run a handler callback
    ## the environment does not remember the loaded packages
    require(gWidgetsWWW, quietly=TRUE)

    ## may need to quiet down via sink
    sink(f <- tempfile())
    if(is.null(POST$context)) {
      out <- try(e[[w]]$runHandler(POST$id), silent=TRUE)
    } else {
      if(exists("sessionDBIlogfile"))
        cat(sessionID, "POST context:", POST$context, "\n", file=sessionDBIlogfile, append=TRUE)

      context <- try(fromJSON(as.character(POST$context)), silent=TRUE)
      out <- try(e[[w]]$runHandler(POST$id, context), silent=TRUE)
    }
    sink()
    
    if(exists("sessionDBIlogfile"))
      cat(sessionID, "ran handler", "\n", file=sessionDBIlogfile, append=TRUE)


    if(inherits(out, "try-error")) {
      unlink(f)
      sendError(db, out)
    } else {
      ## read out
      setContentType("text/plain")
      out <- try(readLines(f), silent=TRUE)
      if(!inherits(out,"try-error"))
        cat(out)
      unlink(f)
      ## store session
      db[[sessionID]] <- e
    }
  } else if(type == "assign") {
    ## Assign a value in the gwindow object
    ## only names of  gWidgetsXXX are permitted

    if(exists("sessionDBIlogfile"))
      cat(sessionID, "assign value", POST$value, "\n", file=sessionDBIlogfile, append=TRUE)
    
    variable <- POST$variable
    value <- POST$value
    value <- fromJSON(value)            # store as JSON object
    value <- value$value                # JSON object is a list(value=xxx)
    ## check that variable matches
    ## Could put in check to limit size of value, o/w the post
    ## data could be arbitrarily large
    if(length(grep("^gWidgetID", variable)) > 0) {
      sink(type="message")                  # quiet RApache errors ## was tempfile()
      tmp <- e[[w]]
      out <- try(assign(variable, value, envir=tmp), silent=TRUE) ## not e[[w]] here.
      if(inherits(out,"try-error")) {
        if(exists("sessionDBIlogfile"))
          cat(sessionID, "assign had error:", out, "\n", file=sessionDBIlogfile, append=TRUE)
      }
      e[[w]] <- tmp
      ## save session
      db[[sessionID]] <- e
      try(sink(NULL), silent=TRUE)
    } else {
      sendError(db, paste("var name ", variable, " is not acceptable"))
    }
  } else if(type == "fileupload") {
    ## XXX Implement me with security
    sink("/tmp/test.txt")
    print(SERVER)
    print(GET)
    print(POST)
    print("----")
    print(file.info(POST[["file-path"]])) # from name of form
    system(sprintf("mv %s /tmp/test.file", POST[['file-path']]))
    sink(NULL)

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
  ## clean up
  if(exists("sessionDBIlogfile"))
      cat(sessionID, "disconnect db", "\n", file=sessionDBIlogfile, append=TRUE)

#  out <- try(dbDisconnect(db), silent=TRUE)
}


DONE
