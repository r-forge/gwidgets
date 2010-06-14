## File to respond to requests like
## http://XXX.XXX.XXX/gWidgetsWWWrun/filename
## Requires a configuration in RApache like
## <Location /gWidgetsWWWrun>
##    SetHandler r-handler
##    RFileHandler /var/www/GUI/gWidgetsWWWrun.R
## </Location>
## filename is found relative to gWidgetsWWWrunBaseDirectory

## These should be configured in RApache config
if(!exists("gWidgetsWWWrunBaseDirectory"))
  gWidgetsWWWrunBaseDirectory <- "/var/www/GUI"

if(!exists("extjsBaseUrl"))
  extjsBaseUrl <- "/ext"


####################################################
## nothing below here to adjust
simpleMsg <- function(msg) {
  setContentType("text/html")
  cat("<html><body>",msg,"</body></html>")
}

findFile <- function(file) {
  for(dir in gWidgetsWWWrunBaseDirectory) {
    tmp <- paste(dir, file, ".R", sep="")
    if(file.exists(tmp))
      return(tmp)
  }
  return("")
}


## check that we have get data
file <- SERVER$path_info
if(is.null(file) || file == "/") file <- "/index"
file <- gsub("\\.R$", "", file)
if(is.null(file))  {
  simpleMsg("No file specified.")
} else if((file <- findFile(file)) == "") {
  simpleMsg("Can't find file")
} else {
  ## create db file db
  if(!file.exists(sessionDbFile))
    dbCreate(sessionDbFile, type=sessionDbType)
  db <- initDb(sessionDbFile, type=sessionDbType)
  ..e <- new.env()
  sessionID <- newSession(db, e=..e) ## XXX what is e?

  
  ## create output
  setContentType("text/html")
  out <- String() ## gWidgetsWWW is loaded in
  out <- out +
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' +
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' +
        '<html xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml">'
  out <- out +
    '<head>' +
      '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'
  out <- out +
#    '<script type="text/javascript" src="' + extjsBaseUrl +'/adapter/ext/ext-base.js"></script>' +
#      '<script type="text/javascript" src="' + extjsBaseUrl + '/ext-all.js"></script>' +
        '<link rel="stylesheet" type="text/css" href="' + extjsBaseUrl + '/resources/css/ext-all.css">'

  if(exists("gWidgetsWWWGoogleAPI") && !is.null(gWidgetsWWWGoogleAPI)) {
    out <- out +
      '<script type="text/javascript" src="http://www.google.com/jsapi?key=' +
        gWidgetsWWWGoogleAPI + '"></script>' +
          '<script type="text/javascript">  google.load("maps", "2"); </script>' + '\n'
  }

  ### We removed the issue with IE by not adding button classes
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


  ## this is for processing code -- isn't working without Rpad
##   out <- out +
##     '<script type="text/javascript">' +
##       paste(readLines(system.file("javascript","processing.js", package="gWidgetsWWW")), collapse="\n") +
##         paste(readLines(system.file("javascript","processinginit.js", package="gWidgetsWWW")), collapse="\n") +
##           '</script>'
  
  out <- out +
    '</head>'

  out <- out +
#    '<body onload=detectBrowser() onunload=clearSession("' + sessionID + '")>'
    '<body onunload=clearSession("' + sessionID + '")>' 

  ## add throbber
  out <- out +
    paste(
          "<div id='loading'>",
          "<div class='loading-indicator'>gWidgetsWWW:<br /><span id='loading-msg'>Loading styles and images...</span></div>",
          "</div>",
          "<span id='loading-msg'></span></div>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Core API...';</script>",
          "<script type='text/javascript' src='", extjsBaseUrl, "/adapter/ext/ext-base.js'></script>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading UI Components...';</script>",
          
          "<script type='text/javascript' src='", extjsBaseUrl, "/ext-all.js'></script>",
          "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",               
          "<script type='text/javascript' src='/gWidgetsWWW.js'></script>",
          "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>",
          sep="")
  
          ## This is now donw in gWindow
##   out <- out +
##     '<script type="text/javascript">' +
##       'var sessionID="' + sessionID + '"' +
##         '</script>'
  out <- out +
    '<script type="text/javascript">'

  cat(out)
  
  ## This wraps in try and eval so that values are stored in the session
  try(eval(substitute({
    source(file, local=TRUE)
  }), envir=..e))

  cat("</script>")




  ## This saves the session and disconnects the data base
  saveSession(db, sessionID, ..e)
  try(dbDisconnect(db), silent=TRUE)

  cat("</BODY></HTML>\n")
}

## send this out
DONE
