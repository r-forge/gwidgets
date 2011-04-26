##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' @include array.R
NA


##' String class -- does not get escaped in object literals
##'
##' Useful in the toJSObject method
##' @param x character vector to coerce to String class
##' @return vector with new class addedd
##' @export
String <- function(x) {
  class(x) <- c("String", class(x))
  x
}


## map R objects into object literals


##' Coerce an object to a string
##'
##' @param x object to coerce
##' @return returns object sometimes quoted
##' @export
##' @rdname gWidgetsWWW2-generics
coerceToJSString <- function(x) UseMethod("coerceToJSString")

##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" default
##' @S3method "coerceToJSString" default
##' @nord
coerceToJSString.default <- function(x) x # no quote
##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" character
##' @S3method "coerceToJSString" character
##' @nord
coerceToJSString.character <- function(x) ourQuote(x)
##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" factor
##' @S3method "coerceToJSString" factor
##' @nord
coerceToJSString.factor <- function(x) ourQuote(as.character(x))
##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" logical
##' @S3method "coerceToJSString" logical
##' @nord
coerceToJSString.logical <- function(x) tolower(as.character(x))
##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" String
##' @S3method "coerceToJSString" String
##' @nord
coerceToJSString.String <- function(x) x # to avoid quoting
##' S3 method for coerceToString
##' @param x object to coerce
##' @method "coerceToJSString" list
##' @S3method "coerceToJSString" list
##' @nord
coerceToJSString.list <- function(x) toJSObject(x)

##' map an R list object into a string containing javascript code representation of an object
##'
##' @param x a list that will map to an object literal. E.g., list(a=1, b="a") goes to {a:1, b:'a'}
##' @return a string with the list formatted as code to produce a JavaScript object
##' @export
##' @examples
##' arg_list <- list(
##'   tooltip = "some tooltip",
##'   width = 200,
##'   height = "auto",
##'   id = String("noquote")
##' )
##' toJSObject(arg_list)
##' 
toJSObject <- function(x) {
  out <- c()

  for(i in names(x)) {
    val <- x[[i]]
    if(is.null(val) || (length(val) == 0) || (length(val) == 1) && is.na(val)) {
      ## nada
    } else if(is.list(val)) {
      out[i] <- toJSObject(val)
    } else {
      out[i] <- coerceToJSString(val)
    }
  }
  nms <- names(out); ind <- grepl("-", nms) # a-b-c -> "a-b-c"
  nms[ind] <- sprintf('"%s"', nms[ind])
  sprintf("{%s}", paste(nms, out, sep=":", collapse=","))
}

## ## coerce a single value to javascript with quotes
## ## logical is buggy
## toJS <- function(x) UseMethod("toJS")
## toJS.default <- function(x) shQuoteEsc(x)
## toJS.logical <- function(x) tolower(as.character(x))
## toJS.integer <- toJS.numeric <- function(x) x
## toJS.factor <- function(x) toJS(as.character(x))


##' An empty array
##'
##' Used by toJSArray as helper function
##' @param doBrackets logical
##' @export
emptyJSArray <- function(doBrackets=TRUE)  ifelse(doBrackets, "[]", "")

##' Make a JS array from an R object
##'
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @return JSON encoded value from arrary
##' @export
toJSArray <- function(x, doBrackets=TRUE) UseMethod("toJSArray")

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" default
##' @S3method "toJSArray" default
##' @nord
toJSArray.default <- function(x, doBrackets=TRUE) stop("no default method")

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" numeric
##' @S3method "toJSArray" numeric
##' @nord
toJSArray.numeric <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- as.character(x)
  x[is.na(x)] <- "'NA'"
  out <- paste(x, collapse=",")
  if(doBrackets)
    out <- paste("[",out,"]", sep="")
  return(out)
}

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput
##' 
##' @method "toJSArray" String
##' @S3method "toJSArray" String
##' @nord
toJSArray.String <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(x, collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" logical
##' @S3method "toJSArray" logical
##' @nord
toJSArray.logical <- function(x,doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- tolower(as.character(x))
  x[is.na(x)] <- "'NA'"
  toJSArray.String(x, doBrackets)
}

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" character
##' @S3method "toJSArray" character
##' @nord
toJSArray.character <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- sprintf("%s", ourQuote(x))
  toJSArray.String(x, doBrackets)
}

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" factor
##' @S3method "toJSArray" factor
##' @nord
toJSArray.factor <- toJSArray.character

##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" matrix
##' @S3method "toJSArray" matrix
##' @nord
toJSArray.matrix <- function(x, doBrackets=TRUE) {
  out <- paste(apply(x,1,toJSArray), collapse=",")
  if(doBrackets) out <- paste("[", out, "]", sep="")
  return(out)
}


##' ToJSArray method  
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" list
##' @S3method "toJSArray" list
##' @nord
toJSArray.list <- function(x, doBrackets=TRUE) {
  sapply(x, function(i) toJSArray(i,doBrackets))
}
       
## This needs work
##' ToJSArray method
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @method "toJSArray" data.frame
##' @S3method "toJSArray" data.frame
##' @nord
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






##' escaping strings
##' 
##' we use shQuote as a convenience for
##' word -> 'word' however, it doesn't escape values as we would like, hence
##' this one.
##' @param x character
##' @return character has single quotes escaped
##' @nord
shQuoteEsc <- function(x) {
  out <- gsub("\'","\\\\'",x)
  out <- paste("'",out,"'",sep="")
  return(out)
}
 
##' make a string safe to pass in as HTML fragment.
##'
##' We pass in strings as 'string contents' so we replace all ' quotes with " ones, then wrap in ''
##' @param x a string to replace ' with
##' @return a string with quotes escaped and placed within ''
##' @nord
ourQuote <- function(x) {
  x <- gsub("'",'"',x)
  x <- gsub("\n", "\\\\n", x)           # \n's are issue
  sprintf("'%s'", x)
}



##' get value from ... arguments
##' @param key key to lookip
##' @param ... passed from function
##' @param default default value if NULL
##' @nord
getFromDots <- function(key, ..., default=NULL) {
  val <- list(...)[[key]]
  if(is.null(val) && !is.null(default))
    default
  else
    val
}

##' Get an object using default if otherwise not defined
##'
##' if val is null etc, return default, else val
##' @param val value to get
##' @param default default to use if null, NA or 0 length
##' @nord
getWithDefault <- function(val, default) {
  if(is.null(val) || is.na(val) || length(val) == 0)
    default
  else
    val
}

##' merge two lists
##' @param x a list
##' @param y a list
##' @param overwrite logical should we overright values in x
##' @nord
merge.list <- function(x, y, overwrite=TRUE) {
  if(missing(y) || is.null(y))
    return(x)
  for(i in names(y))
    if(overwrite || !(i %in% names(x)))
      x[[i]] <- y[[i]]
  x
}

##

##' Is value a URL: either of our class URL or matches url string: ftp://, http:// or file:///
##'
##' @param x length 1 character value to test
##' @return Logical indicating if a URL.
##' @export
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
##' @export
asURL <- function(x) {
  if(!is(x,"URL"))
    class(x) <- c("URL",class(x))
  return(x)
}


##' Get a temporary file matching the given extension, if given.
##'
##' This tempfile is stored in a directory that can be served as a static file through
##' gWidgetsWWW2.
##' @param ext file extension
##' @export
##' @examples
##' f <- get_tempfile(ext=".svg")
##' get_tempfile_url(f)
get_tempfile <- function(ext=".txt") {
  dir.create(sprintf("%s%s%s",tempdir(), .Platform$file.sep, "tmp"), showWarnings=FALSE)
  f <- tempfile(pattern="gWidgetsWWW", tmpdir="", fileext=ext)
  f <- sprintf("%s/tmp%s", tempdir(), f)
  class(f) <- c("StaticTempFile", class(f))
  f
}

##' Get url for a tempfile created by \code{get_tempfile}
##'
##' Requires the tempdir to be mapped to a specific url
##' @param f a file name produced by \code{get_tempfile}
##' @export
##' @examples
##' f <- get_tempfile(ext=".svg")
##' get_tempfile_url(f)
get_tempfile_url <- function(f) {
  if(!is(f, "StaticTempFile"))
    return(f)
  
  sprintf("/custom/tmp/tmp/%s", basename(f))
}



##' load a web app defined by a gWidgetsWWW2 script
##'
##' @param script_name path to gWidgetssWWW2 script
##' @param app_name base name for script, unique per project
##' @param brew_template The script may render to the entire page, or
##' parts of the page specified by an ID. The allows one to specify a
##' brew template to hold these place holders and other HTML code.
##' @param use.googlemap If using \code{ggooglemaps} include this so that the JavaScript files are downloaded.
##' @param show.log If TRUE, logged information is written to the console
##' @param ... passed to \code{brew} call of \code{brew_template}
##' @export
##' @examples
##' ## open an app that takes the entire page
##' gw_script <-  system.file("examples/hello-world.R", package="gWidgetsWWW2")
##' load_app(gw_script, "HelloApp",  use.google=FALSE)
##' ## open an app using googlemaps
##' load_app(system.file("examples/ggooglemaps.R", package="gWidgetsWWW2"),  "GoogleMapApp", use.google=TRUE)
##' ## open an app embedded in another page
##' gw_script <- system.file("examples/multiple.R", package="gWidgetsWWW2")
##' brew_template <- system.file("framework/brew/custom.rhtml", package="gWidgetsWWW2")
##' load_app(gw_script, "MultipleApp",  brew_template)
load_app <- function(script_name,
                     app_name="test",
                     brew_template = "",
                     use.googlemap = FALSE,
##                     use.googleVis=TRUE,
                     show.log=FALSE,
                     ...
                         ) {

  options("Rhttpd_debug"=as.logical(show.log))
  
  
  R <- Rhttpd$new()
  try(R$start(), silent=TRUE)

  ## extra html code googlemaps, ace?, ...
  extra_html_code <- character(0)
  if(use.googlemap)
    extra_html_code <- c(extra_html_code,
                         '<script type="text/javascript" ',
                         'src="http://www.google.com/jsapi?autoload={\'modules\':[{name:\'maps\',version:3,other_params:\'sensor=false\'}]}">',
                         '</script>',
                         '<script type="text/javascript" src="/custom/gWidgetsWWW2/javascript/ext.ux.GMapPanel3.js"></script>'
                         )

  ## XXX Set up for using google visualization through googleVis. First attempt didn't work.
  ## if(use.googleVis)
  ##   extra_html_code <- c(extra_html_code,
  ##                        '<script type="text/javascript" src="http://www.google.com/jsapi"></script>'
  ##                        )
                         

  
  ## gWidgetsWWW, static files
  gWidgetsWWW <- Rook::Static$new(
                                  urls = c("/images", "/javascript", "/css"),
                                  root = system.file("base", package="gWidgetsWWW2")
                                  )
  R$add(RhttpdApp$new(gWidgetsWWW, name="gWidgetsWWW2"))
  
  ## tmpdir
  tmpApp <- Rook::Static$new(
                             urls=c("/tmp"),
                             root=tempdir()
                             )
  R$add(RhttpdApp$new(tmpApp, name="tmp"))
  
  ## brew index
  brewery <- Rook:::Brewery$new(url="/",
                                root=system.file("framework/brew", package="gWidgetsWWW2"),
                                app_name = app_name,
                                brew_template=brew_template,
                                extra_html_code = paste(extra_html_code, collapse="\n"),
                                ...
                                )
                                        #  R$add(RhttpdApp$new(brewery, name="gwbrew"))
  
  ## an application
  app <- Rook::Builder$new(
                           ## app specific static files
                           Rook:::Static$new(
                                             urls = c('/css','/images','/javascript'), 
                                             root = '.'
                                             ),
                           ## brew files
                           brewery,
                           ## Rook:::Brewery$new(
                           ##                    url=sprintf("%s/brew", app_name),
                           ##                    root='.'
                           ##                    ),
                           ## why do I need this?
                           tmpApp,
                           gWidgetsWWW2:::GWidgetsApp$new(url="/gwapp", script=script_name),
                           ## why does this fail?
                           Rook:::Redirect$new(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
                           )
  
  R$add(RhttpdApp$new(app, name=app_name))
  
  browseURL(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
  
}

