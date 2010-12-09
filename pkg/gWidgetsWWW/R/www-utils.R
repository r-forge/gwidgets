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

##' Encode a URL
##'
##' like URLencode, but takes care of plus signs
##' @param x character
##' @return character. Has entities inserted
##' @export
ourURLencode <- function(x) {
  ## handle + signs too
  x <- URLencode(x)
  x <- gsub("+","&2B;", x, fixed=TRUE)
  x
}

##' Decode a URL
##'
##' same as URLdecode, but takes care of plus signs
##' @param x character
##' @return calls URLdecode then decodes plus signs
ourURLdecode <- function(x) {
  if(is.null(x))
    return(x)
  x <- URLdecode(x)
  x <- gsub("&2B;", "+", x, fixed=TRUE)
  x
}


##' function to escapeHTML characters
##'
##' @param x character
##' @return character. Replaces characters with HTML entitities
##' @export
escapeHTML <- function(x) {
  translations <- function(i) {
    switch(i,
           '<' = "&lt;",
           '>' = "&gt;",
           '&' = "&amp;",
           '"' = "&quot;",
           "'" = "&#39;",
           ## nee3d ASCII equivalents
           ##            'à' = "&agrave;",
##            'À' = "&Agrave;",
##            'â' = "&acirc;",
##            'Â' = "&Acirc;",
##            'ä' = "&auml;",
##            'Ä' = "&Auml;",
##            'å' = "&aring;",
##            'Å' = "&Aring;",
##            'æ' = "&aelig;",
##            'Æ' = "&AElig;",
##            'ç' = "&ccedil;",
##            'Ç' = "&Ccedil;",
##            'é' = "&eacute;",
##            'É' = "&Eacute;",
##            'è' = "&egrave;",
##            'È' = "&Egrave;",
##            'ê' = "&ecirc;",
##            'Ê' = "&Ecirc;",
##            'ë' = "&euml;",
##            'Ë' = "&Euml;",
##            'ï' = "&iuml;",
##            'Ï' = "&Iuml;",
##            'ô' = "&ocirc;",
##            'Ô' = "&Ocirc;",
##            'ö' = "&ouml;",
##            'Ö' = "&Ouml;",
##            'ø' = "&oslash;",
##            'Ø' = "&Oslash;",
##            'ß' = "&szlig;",
##            'ù' = "&ugrave;",
##            'Ù' = "&Ugrave;",        
##            'û' = "&ucirc;",      
##            'Û' = "&Ucirc;",
##            'ü' = "&uuml;",
##            'Ü' = "&Uuml;",
##            '®' = "&reg;",       
##            '©' = "&copy;",   
##            '€' = "&euro;",
##              ' ' = "&nbsp;",
           i)
  }
  tmp <- unlist(strsplit(x, ""))
  tmp <- sapply(tmp, translations)
  x = paste(tmp, collapse="")
  return(x)
}

##' reverse for escapeURL
##'
##' @param x character
##' @return character
unescapeURL <- function(x) {
  codes <- c("%20" = " ",
             "%22" = '"',
             "%3C" = "<",
             "%3E" = ">",
             "%23" = "#",
             "%25" = "%",
             "%28" = "(",
             "%29" = ")",
             "%2B" = "+",
             "%2C" = ",",
             "%7B" = "{",
             "%7D" = "}",
             "%7C" = "|",
             "%5C" = "\\",
             "%5E" = "^",
             "%7E" = "~",
             "%5B" = "[",
             "%5D" = "]",
             "%60" = "`",
             "%3B" = ";",
             "%2F" = "/",
             "%3F" = "?",
             "%3A" = ":",
             "%40" = "@",
             "%3D" = "=",
             "%26" = "&",
             "%27" = "\\'",
             "%24" = "$",
             "%0A" = "\n")
  
  for(i in names(codes)) 
    x <- gsub(i,codes[i],x)
  return(x)
}

##' strip leading and trailing slashes (/) from character
##' 
##' @param x string to trim
##' @param leading logical. If TRUE stripleading slashes
##' @param trailing logical. If TRUE strip trailing slashes
strip_slashes <- function(x, leading=TRUE, trailing=TRUE) {
  if(leading)
    x <- gsub("^[/]{1,}","",x)
  if(trailing)
    x <- gsub("[/]{1,}$","", x)
  x
}
  
##' make a string safe to pass in as HTML fragment.
##'
##' We pass in strings that work with '....' so we replace ' with \" an d" with \"
##' @param x a string to replace ' with
##' @result string with quotes escaped and placed within ''
ourQuote <- function(x) {
  x <- gsub("'",'"',x)
  sprintf("'%s'", x)
}

##' strip off \n and turn ' into \' so that value can be assigned withing javascript call
##'
##' Used by ghtml, glabel, gtext, ...
##' @param x a character vector
##' @param encode do we escape HTML bits
stripSlashN <- function(x, encode=FALSE, sep=c("\\n", "<br />"), dostrwrap=TRUE) {
  x <- gsub("\n"," ", x)
  x <- gsub("'", "\\\\'",x)
  if(dostrwrap) {
    x <- paste(x, collapse="")
    x <- strwrap(x)
  }
  if(encode)
    x <- gWidgetsWWW:::escapeHTML(x)
  x <- paste(x, collapse=sep)

  x
}

##################################################
## Helpers
## see source defn.
##' Is value a URL: either of our class URL or matches url string: ftp://, http:// or file:///
##'
##' @param x length 1 character value to test
##' @return Logical indicating if a URL.
isURL <- function(x) {

  ## we can bypass this by setting a value to have this class
  ## as in isURL((class(x) <- "URL"))
  if(is(x,"URL")) return(TRUE)
  if (is.character(x) && length(x) == 1) 
    out <- length(grep("^(ftp|http|file)://", x)) > 0
 else
   out <- FALSE
  return(out)
}

##' Add URL to class of object if not already
##'
##' @param x object to add class to. Should be length 1 character
##' @return returns object
asURL <- function(x) {
  if(!is(x,"URL"))
    class(x) <- c("URL",class(x))
  return(x)
}


##################################################
## JSON stuff

##' take value from JSON
##'
##' Same as rjon's fromJSON only deals with ""
##' @param x character string containin JSON code
##' @param ... passed to fromJSON
##' @return character
##' @export
ourFromJSON <- function(x, ...) {
  if(x == "") return(x)
  fromJSON(x, ...)
}

##' make toJSON a method
##' Failed? Wasn't dispatching right, so hard code in classes
ourToJSON <- function(x, ...) {
  f <- function(x) {
    if(is(x, "logical"))
      x <- tolower(as.character(x))
    if(is(x, "factor"))
      x <- as.character(x)
    if(is(x, "character"))
      x <- shQuote(x)
    
    sprintf("[%s]", paste(as.character(x), collapse=","))
  }
  if(is(x, "data.frame"))
    out <- sprintf("[%s]",
            paste(shQuote(names(x)), sapply(x, f), sep=":", collapse=","))
  else
    out <- f(x)
}

##' coerce an object into a JSStrig
## String here is misnamed --
## this function creates JS values
coerceToJSString <- function(x) UseMethod("coerceToJSString")
coerceToJSString.default <- function(x) x # no quote
coerceToJSString.character <- function(x) shQuoteEsc(x)
coerceToJSString.factor <- function(x) shQuoteEsc(as.character(x))
coerceToJSString.logical <- function(x) tolower(as.character(x))
coerceToJSString.function <- function(x) coerceToJSString(x())
coerceToJSString.String <- function(x) x # to avoid quoting



## coerce a single value to javascript with quotes
## logical is buggy
toJS <- function(x) UseMethod("toJS")
toJS.default <- function(x) shQuoteEsc(x)
toJS.logical <- function(x) tolower(as.character(x))
toJS.integer <- toJS.numeric <- function(x) x
toJS.factor <- function(x) toJS(as.character(x))


##' Make a JS array from an R object
##'
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @return JSON encoded
emptyJSArray <- function(doBrackets=TRUE)  ifelse(doBrackets, "[]", "")
toJSArray <- function(x, doBrackets=TRUE) UseMethod("toJSArray")
toJSArray.default <- function(x, doBrackets=TRUE) stop("no default method")
toJSArray.integer <- toJSArray.numeric <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- as.character(x)
  x[is.na(x)] <- "'NA'"
  out <- paste(x, collapse=",")
  if(doBrackets)
    out <- paste("[",out,"]", sep="")
  return(out)
}
toJSArray.factor <- toJSArray.character <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(shQuoteEsc(as.character(x)), collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}
toJSArray.String <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(x, collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}

toJSArray.logical <- function(x,doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- tolower(as.character(x))
  x[is.na(x)] <- "'NA'"
  toJSArray.String(x, doBrackets)
}

toJSArray.character <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- sprintf("%s", ourQuote(x))
  toJSArray.String(x, doBrackets)
}

toJSArray.matrix <- function(x, doBrackets=TRUE) {
  out <- paste(apply(x,1,toJSArray), collapse=",")
  if(doBrackets) out <- paste("[", out, "]", sep="")
  return(out)
}


  
toJSArray.list <- function(x, doBrackets=TRUE) {
  sapply(x, function(i) toJSArray(i,doBrackets))
}
       
## This needs work
toJSArray.data.frame <- function(x,doBrackets=TRUE) {
  if(nrow(x) == 0) {
    n <- ncol(x)
    out <- paste(rep("[]", n), collapse=",")
    if(doBrackets)
      out <- sprintf("[%s]", out)
    return(out)
  }
  ## depends on number of cols
  if(ncol(x) == 1)
    return(toJSArray(x[,1,drop=TRUE]))

  ## otherwise, we need to work
  tmp <- sapply(x, function(y) toJSArray.list(y, doBrackets=FALSE))
  if(!is.matrix(tmp))
    tmp <- matrix(tmp, ncol=length(tmp))

  tmp1 <- apply(tmp,1,function(i) paste("[",paste(i,collapse=","),"]",sep=""))
  out <- paste(tmp1, collapse=",")
  if(doBrackets) out <- paste("[",out,"]",sep="")
  return(out)
}

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

  
  if(grepl(gWidgetsWWWStaticDir, val, fixed=TRUE))
    val <- gsub(gWidgetsWWWStaticDir, gWidgetsWWWStaticUrlBase, val, fixed=TRUE) # fixed!

  ourURLencode(val)
}

