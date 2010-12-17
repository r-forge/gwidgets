## File to respond to requests like
## http://XXX.XXX.XXX/gWidgetsWWWrun/filename

## Requires a configuration in RApache like
## <Location /gWidgetsWWWrun>
##    SetHandler r-handler
##    RFileHandler /var/www/GUI/gWidgetsWWWrun.R
## </Location>

## should be configured in rapache.conf
if(!exists("extjsBaseUrl"))
  extjsBaseUrl <- "/ext"

####################################################
## nothing below here to adjust
simpleMsg <- function(msg) {
  setContentType("text/html")
  cat("<html><body>",msg,"</body></html>")
}

##' find file among the directories specified in the RApache-gWidgetsWWW.conf file
##'
##' @param file filename with ".R" stripped off
##' @return either the path of the file, or a length 0 character vector
findFile <- function(file) {
  dirs <- getOption("gWidgetsWWWrunBaseDirectory")
  dirs <- gsub("/{1,}$","",dirs) ## remove slashes
  file <- gsub("^/{1,}","",file)
  file <- gsub("[/]*$", "", file)       # trailing slash
  files <- file.path(dirs, paste(file, ".R", sep=""))
  ind <- sapply(files, file.exists)
  return(head(files[ind], n=1))         # length == 0 if none
}



## check that we have get data
file <- SERVER$path_info
if(is.null(file) || file == "/") file <- "/index"
file <- gsub("[/]*$", "", file)       # trailing slash
file <- gsub("\\.R$", "", file)
if(is.null(file))  {
  simpleMsg("No file specified.")
} else if(length(file <- findFile(file)) == 0) {
  simpleMsg(sprintf("Can't find file %s", SERVER$path_info))
} else {
  ## create db file db
  if(!file.exists(sessionDbFile))
    dbCreate(sessionDbFile, type=sessionDbType)
  db <- initDb(sessionDbFile, type=sessionDbType)
  ..e <- new.env()
##  sessionID <- newSession(db, e=..e) ## XXX what is e?

  
  ## create output
  setContentType("text/html")
  ## set cache headers
#  setHeader("Cache-Control", "max-age=31536000")
#  setHeader("Last-Modified", format(file.info(file)$mtime,"%a %b %e %Y %H:%M:%S GMT%z (%Z)"))
#  setHeader("Expires", format(Sys.time() + 60*60*24, "%a %b %e %Y %H:%M:%S GMT%z (%Z)"))
            
  out <- String() ## gWidgetsWWW is loaded in
  out <- out +
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' +
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' +
        '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml">' +
          '\n'
  out <- out + "\n" +
    '<head>' +
      '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">' +
        '\n'
  
  out <- out +
    '<link rel="stylesheet" type="text/css" href="' + extjsBaseUrl + '/resources/css/ext-all.css">' +
      '\n'

  ## if(exists("gWidgetsWWWGoogleAPI") && !is.null(gWidgetsWWWGoogleAPI)) {
  ##   out <- out +
  ##     '<script type="text/javascript" src="http://www.google.com/jsapi?key=' +
  ##       gWidgetsWWWGoogleAPI + '"></script>' +
  ##         '<script type="text/javascript">  google.load("maps", "2"); </script>' + '\n'
  ## }

  ## We removed the issue with IE by not adding button classes
  ##   out <- out +
  ##     '<script type="text/javascript">' +
  ##       'function detectBrowser() {' +
  ##         'var browser=navigator.appName;' +
  ##           'var b_version=navigator.appVersion;' +
  ##             'var version=parseFloat(b_version);' +
  ##               'if (browser=="Microsoft Internet Explorer") {' +
  ##                 'alert("gWidgetsWWW does not work with Internet Explorer. Try Google Chrome, Firefox, Opera, ...");' +
  ##                   'return(false);' +
  ##                   '}};' + '\n'

  ##   ## override detect browser for testing
  ##   out <- out +
  ##     'function detectBrowser() {};'
  
  ##   out <- out + '</script>'


  
  out <- out +
    '</head>' + '\n'

#  out <- out +
#    '<body onunload=clearSession("' + sessionID + '")>' 

  ## add throbber
  out <- out +
    paste(
          "<div id='loading'>",
          "<div class='loading-indicator'>gWidgetsWWW:<br /><span id='loading-msg'>Loading styles and images...</span></div>",
          "</div>", "\n",
          "<span id='loading-msg'></span></div>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Core API...';</script>",
          "\n",
          "<script type='text/javascript' src='", extjsBaseUrl, "/adapter/ext/ext-base.js'></script>",
          "\n",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Ext UI Components...';</script>",
          "\n",
          "<script type='text/javascript' src='", extjsBaseUrl, "/ext-all.js'></script>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading ext extra libraries...';</script>",
          "\n",
          "<script type='text/javascript' src='", extjsBaseUrl, "/examples/ux/ux-all.js'></script>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",
          "\n",
          "<script type='text/javascript' src='",gWidgetsWWWjsUrl, "'></script>",
          "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>",
          "\n",
          sep="")

  cat(out)
  

  ## print out web page by gWidgets
  cat('<script type="text/javascript">\n')
  ## This wraps in try and eval so that values are stored in the session
  try(eval(substitute({
    source(file, local=TRUE)
  }), envir=..e))
  cat("</script>\n")


  ## get sessionID from environment. It is defined in displaying gwindow
  nms <- ls(..e)
  ind <- sapply(nms, function(i) {
    is(get(i, envir=..e), "gWindow")
  })
  if(any(ind)) {
    w <- get(nms[min(which(ind))], envir=..e)
    ## This saves the session and disconnects the data base
    saveSession(db, w$sessionID, ..e)
    try(dbDisconnect(db), silent=TRUE)
  } else {
    ## XXX a big error here -- no sessionID
  }


  cat("</BODY></HTML>\n")
}

## send this out
DONE
