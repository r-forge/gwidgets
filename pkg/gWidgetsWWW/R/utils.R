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

## utility functions

##' Get a static file that can be served by the browser
##'
##' @param ext file extension. Leading dot unnecessary
##' @param filename If given uses this filename, otherwise calls tempfile
##' @return the the file name.
##' @seealso \code{\link{convertStaticFileToUrl}} to get corresponding url for serving through the browser.
##' @examples
##' ## a basic usage:
##' \dontrun{
##' f <- getStaticTempFile(".svg")
##' svg(f)
##' hist(rnorm(100))
##' dev.off
##' svalue(gsvg_instance) <- convertStaticFileToUrl(f)
##' }
##' @export
getStaticTmpFile <- function(ext="", filename)  {
  if(gWidgetsWWWIsLocal()) {
    gWidgetsWWWStaticDir <- get("gWidgetsWWWStaticDir", envir=.GlobalEnv)
  } else {
    gWidgetsWWWStaticDir <- getOption("gWidgetsWWWStaticDir")
  }

  try(dir.create(gWidgetsWWWStaticDir, showWarnings=FALSE), silent=FALSE)
  ext <- gsub("^[.]{1,}","",ext)        # remove do if there
  
  if(missing(filename)) {
    out <- paste(tempfile(tmpdir=gWidgetsWWWStaticDir),ext,sep=".")
  } else {
    filename <- sprintf("%s.%s", filename, ext)
    out <- file.path(gWidgetsWWWStaticDir,filename)
  }
  return(out)
}

##' convert static file from local file system to url for serving in browser
##'
##' @param val filename, usuallly given by getStaticTmpFile
##' @return a url to serve through browser
##' @export
convertStaticFileToUrl <- function(val) {
  if(gWidgetsWWWIsLocal()) {
    gWidgetsWWWStaticDir <- get("gWidgetsWWWStaticDir", envir=.GlobalEnv)
    gWidgetsWWWStaticUrlBase <- get("gWidgetsWWWStaticUrlBase", envir=.GlobalEnv)
  } else {
    gWidgetsWWWStaticDir <- getOption("gWidgetsWWWStaticDir")
    gWidgetsWWWStaticUrlBase <- getOption("gWidgetsWWWStaticUrlBase")
  }
  ## strip off static dir from val, append on static url base
#  val <- gsub(gWidgetsWWWStaticDir, gWidgetsWWWStaticUrlBase, val)

  cat("Static\n", file="/tmp/debug-convert.txt")
  cat(val, "\n", file="/tmp/debug-convert.txt", append=TRUE)
  
  if(grepl(gWidgetsWWWStaticDir, val, fixed=TRUE))
    val <- gsub(gWidgetsWWWStaticDir, gWidgetsWWWStaticUrlBase, val, fixed=TRUE) # fixed!

  cat(val, "\n", file="/tmp/debug-convert.txt", append=TRUE)
  ourURLencode(val)
}

##' make a session id for keeping track of different instances
##'
##' @return character. a session id
makeSessionID <- function() {
  ## get key
  key <- "123456"
  if(!is.null(tmp <- getOption("sessionSecretKey"))) {
    key <- tmp
  } else if(exists("sessionSecretKey", envir=.GlobalEnv)) {
    key <- get("sessionSecretKey", envir=.GlobalEnv)
  }
  txt <- as.character(Sys.time())
  key <- paste(key, txt, sep="")
  ID <- digest(key, algo="md5")
  return(ID)
}



##' are we online?
##'
##' Returns TRUE is online. Deprecate?
gWidgetsWWWIsOnline <- function() FALSE

##' bypass require so that we can put optional packages in different fields in DESCRIPTION
##'
##' From Henrik Bengtsson
##' @param pkg package name
bypassRequire <- function(pkg) {
  path <- system.file(package=pkg);
  (path != "");
}
