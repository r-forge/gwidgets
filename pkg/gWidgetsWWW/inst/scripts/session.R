## Basic API for sessions
## we use filehashSQLite to store sessions by a key
## sessions are R environments

## These global values are configured in RApache.conf
#sessionDbFile = "/tmp/testdb" ## no .'s!!!
#sessionDbType = "SQLite"
#sessionSecretKey = "abcdefg"
#sessionTimeout <- 10000 # in seconds or NULL

sessionMaxTries <- 10

## init
## close
## dbDisconnect(db)
## API

## add skip to file exits error handling to dbCreate
createDb <- function(file=sessionDbFile) {
  if(!file.exists(file)) {
     out <- try(dbCreate(file, type=sessionDbType), silent=TRUE)
     if(inherits(out,"try-error"))
       stop(out)
   }
  return(TRUE)

}

## wrap dbInit in a loop to keep trying if there is an error
initDb <- function(file=sessionDbFile, type=sessionDbType) {
  ctr <- 1
  db <- try(dbInit(file, type=type), silent=TRUE)
  if(ctr < sessionMaxTries && inherits(db, "try-error")) {
    Sys.sleep(0.1)    ## 100ms
    ctr <- ctr + 1
    db <- try(dbInit(file, type=type), silent=TRUE)
  }
  return(db) ## warts and all if failed
}

## session API
##' Is this key still valid
##'
##' return list with retval componet and reason component
##' @param db database object
##' @param key sessionID
validKey <- function(db, key) {
  ## return(list(retval=TRUE)) ## DEBUG
  val <- try(dbExists(db, key), silent=TRUE)
  if(inherits(val, "try-error"))
     return(list(retval=FALSE, reason="no such key"))
  if(!val)
    return(list(retval=FALSE, reason="no such key"))
  ## check that not timed out
  e <- db[[key]]
  if(sessionHasTimedout(e)) {
     return(list(retval=FALSE, reason="Session has expired"))
  }
  return(list(retval=TRUE))
}
             
getSession <- function(db, key) {
  if(validKey(db, key)$retval) ## XXX fix me
   db[['key']]
  else
    return(list(retval=FALSE, reason="Key not valid"))
}
saveSession <- function(db, key, e) {
  db[[key]] <- e
}

## make a new session, return key
newSession <- function(db, e=new.env(), key) {
  if(missing(key))
    key <- makeSessionID()
  e$.timeOut <- Sys.time() + sessionTimeout
  saveSession(db, key, e)
  return(key)
}

## shortcut
evalInSession <- function(key, cmds) {
  db <- try(dbInit(sessionDbFile, type=sessionDbType), silent=TRUE)
  e <- getSession(db, key)
  out <- try(eval(cmds, envir=e), silent=TRUE)
  if(!inherits(out, "try-error")) {
    if(!is.null(sessionTimeout))
      e$.timeOut <- Sys.time() + sesssionTimeout
    setSession(db, key, e)
  } else {
    ## ?? an error in eval
  }
  dbDisconnect(db)
}

sessionHasTimedout <- function(e) {
  exists(".timeOut", e) &&
  e[[".timeOut"]] < Sys.time()
}


## some cleanups
clearConnections <- function() {
  odbc <- dbDriver("SQLite")
  sapply(dbListConnections(odbc), dbDisconnect)
}

## moved into common
## makeSessionID <- function() {
##   txt <- as.character(Sys.time())
##   key <- paste(sessionSecretKey, txt, sep="")
##   ID <- digest(key, algo="md5")
##   return(ID)
## }
