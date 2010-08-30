##' like URLencode, but takes care of plus signs
ourURLencode <- function(x) {
  ## handle + signs too
  x <- URLencode(x)
  x <- gsub("+","&2B;", x, fixed=TRUE)
  x
}

##' same as URLdecode, but takes care of plus signs
ourURLdecode <- function(x) {
  if(is.null(x))
    return(x)
  x <- URLdecode(x)
  x <- gsub("&2B;", "+", x, fixed=TRUE)
  x
}

##' escaping strings
## we use shQuote as a convenience for
## word -> 'word' however, it doesn't escape values as we would like, hence
## this one.
shQuoteEsc <- function(x) {
  out <- gsub("\'","\\\\'",x)
  out <- paste("'",out,"'",sep="")
  return(out)
}



##' function to escapeHTML characters
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
           ' ' = "&nbsp;",
           i)
  }
  tmp <- unlist(strsplit(x, ""))
  tmp <- sapply(tmp, translations)
  x = paste(tmp, collapse="")
  return(x)
}

##' revers for escapeURL
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

##' replace ' with \\'
##' Also can replace ' with &143; type thingy
escapeQuotes <- function(x) {
  for(i in 1:length(x)) {
    chars <- unlist(strsplit(x[i],""))
    ind <- grep("'", chars)
    if(length(ind) > 0) {
      for(j in ind) {
        if(j < 3 || (chars[j-2] != "/" && chars[j-1] != "/"))
          if(gWidgetsWWWIsLocal())
            chars[ind] <- "\\'"
          else
            chars[ind] <- "\'"
      }
      x[i] <- paste(chars, collapse="")
    }
  }
  return(x)
}

ourFromJSON <- function(x, ...) {
  if(x == "") return(x)
  fromJSON(x, ...)
}

###
##' are we online?
gWidgetsWWWIsOnline <- function() FALSE
##' bypass require so that we can put optional packages in different fields in DESCRIPTION
##' From Henrik Bengtsson
bypassRequire <- function(pkg) {
  path <- system.file(package=pkg);
  (path != "");
}
##################################################
## string class

##' String constructor -- gives some methods for character data
##'
##' @param x a string
##' @param sep Passed to \code{paste} call when string is created
##' @param common Passed to \code{paste} call when string is created
##' @return a "String" instance. See is \code{+.String} method
String <- function(x,sep="",collapse="") {
  if(missing(x)) x <- ""
  x <- as.character(x)
  class(x) <- c("String","character")
  attr(x,"..sep") <- sep
  attr(x,"..collapse") <- collapse
  return(x)
}
"+.String" <- function(x,...) {
  sep <- attr(x,"..sep"); collapse <- attr(x,"..collapse")
  out <- paste(x,paste(...,sep="",collapse=""),sep=sep,collapse=collapse)
  invisible(String(out,sep=attr(x,"..sep"), collapse = attr(x,"..collapse")))
}
c.String <- function(x,...) {
  sep <- attr(x,"..sep"); collapse <- attr(x,"..collapse")
  out <- x + paste(..., sep=sep, collapse=collapse)
  return(out)
}
print.String <- function(x,...) cat(x)
length.String <- function(x) nchar(x)
"[.String" <- function(x,i,j,...,drop=TRUE) {
  if(missing(i)) i <- 1:length(x)
  unlist(strsplit(x,""))[i]
}
"[<-.String" <- function(x,i,j,...,value) {
  tmp <- x[]
  if(missing(i))
    tmp <- value
  else
    tmp[i] <- value

  return(String(paste(tmp,collapse="")))
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


## ## coerce R objects into javascript arrays
## quoteIt <- function(x) UseMethod("quoteIt")
## quoteIt.default <- function(x) stop("no default method")
## quoteIt.factor <- quoteIt.character <- function(x) shQuote(as.character(x))
## quoteIt.integer <- quoteIt.numeric <- function(x) as.character(x)
## quoteIt.logical <- function(x) tolower(as.character(x))
## quoteIt.Data <- function(x) shQuote(as.character(x))

## coerce a single value to javascript with quotes
## logical is biggy
toJS <- function(x) UseMethod("toJS")
toJS.default <- function(x) shQuoteEsc(x)
toJS.logical <- function(x) tolower(as.character(x))
toJS.integer <- toJS.numeric <- function(x) x
toJS.factor <- function(x) toJS(as.character(x))




toJSArray <- function(x, doBrackets=TRUE) UseMethod("toJSArray")
toJSArray.default <- function(x, doBrackets=TRUE) stop("no default method")
toJSArray.integer <- toJSArray.numeric <- function(x, doBrackets=TRUE) {
  x <- as.character(x)
  x[is.na(x)] <- "'NA'"
  out <- paste(x, collapse=",")
  if(doBrackets)
    out <- paste("[",out,"]", sep="")
  return(out)
}
toJSArray.factor <- toJSArray.character <- function(x, doBrackets=TRUE) {
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(shQuoteEsc(as.character(x)), collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}
toJSArray.String <- function(x, doBrackets=TRUE) {
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(x, collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}

toJSArray.logical <- function(x,doBrackets=TRUE) {
  x <- tolower(as.character(x))
  x[is.na(x)] <- "'NA'"
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
               

## for working with static html files
getStaticTmpFile <- function(ext="")  {
  if(gWidgetsWWWIsLocal()) {
    gWidgetsWWWStaticDir <- get("gWidgetsWWWStaticDir", envir=.GlobalEnv)
  }
  try(dir.create(gWidgetsWWWStaticDir, showWarnings=FALSE), silent=FALSE)
  out <- paste(tempfile(tmpdir=gWidgetsWWWStaticDir),ext, sep="")
#  out <- gsub("[/]{2,}","/",out)        # strip off doubles
  return(out)
}


convertStaticFileToUrl <- function(val) {
  if(gWidgetsWWWIsLocal()) {
    gWidgetsWWWStaticDir <- get("gWidgetsWWWStaticDir", envir=.GlobalEnv)
    gWidgetsWWWStaticUrlBase <- get("gWidgetsWWWStaticUrlBase", envir=.GlobalEnv)
  }
  if(!grepl(gWidgetsWWWStaticUrlBase, val, fixed=TRUE))
    val <- gsub(gWidgetsWWWStaticDir, gWidgetsWWWStaticUrlBase, val, fixed=TRUE) # fixed!
  ourURLencode(val)
}

## for keeping track of different instances
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


