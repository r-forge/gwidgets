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
  txt <- as.character(as.numeric(runif(1) + Sys.time()))
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


##' get with a default
getWithDefault <- function(x, default) {
  if(is.null(x))
    default
  else
    x
}

##' get from ...
getFromDots <- function(..., var, default) getWithDefault(list(...)[[var]], default)
