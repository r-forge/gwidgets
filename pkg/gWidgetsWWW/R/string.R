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

## String related functions

##################################################
## string class

##' String constructor -- gives some methods for character data
##'
##' A string is a length 1 character vector with additional methods
##' @param x a string
##' @param sep Passed to \code{paste} call when string is created
##' @param common Passed to \code{paste} call when string is created
##' @return a "String" instance. See is \code{+.String} method
String <- function(x,sep="",collapse="") {
  if(missing(x)) x <- ""
  x <- as.character(paste(x, collapse=collapse))
  class(x) <- c("String","character")
  attr(x,"..sep") <- sep
  attr(x,"..collapse") <- collapse
  return(x)
}

##' concatenate strings
##'
##' @param x String class object
##' @param ... added to x
##' @return a String class object
"+.String" <- function(x,...) {
  sep <- attr(x,"..sep"); collapse <- attr(x,"..collapse")
  out <- paste(x,paste(...,sep="",collapse=""),sep=sep,collapse=collapse)
  invisible(String(out,sep=attr(x,"..sep"), collapse = attr(x,"..collapse")))
}

##' combine strings with +
##' @param x a String object
##' @param ... combined with x
##' @return a String object 
c.String <- function(x,...) {
  sep <- attr(x,"..sep"); collapse <- attr(x,"..collapse")
  out <- x + paste(..., sep=sep, collapse=collapse)
  return(out)
}

##' print method for String class
##'
##' @param x String object
##' @param ... ignored
print.String <- function(x,...) cat(x)

##' length method for String class
##'
##' @param x String object
##' @return number of characters in x
length.String <- function(x) nchar(x)
"[.String" <- function(x,i,j,...,drop=TRUE) {
  if(missing(i)) i <- 1:length(x)
  unlist(strsplit(x,""))[i]
}

##' Assign into a string object
##'
##' The string is indexed by position of character
##' @param i indices to slice into. Replaces if not specified
##' @param j ignored
##' @param ... ignored
##' @param value inserts value into this position
"[<-.String" <- function(x,i,j,...,value) {
  tmp <- x[]
  if(missing(i))
    tmp <- String(value)
  else
    tmp[i] <- String(value)

  return(String(paste(tmp,collapse="")))
}


##################################################
## Quotes etc.

##' escaping strings
##' 
##' we use shQuote as a convenience for
##' word -> 'word' however, it doesn't escape values as we would like, hence
##' this one.
##' @param x character
##' @return character has single quotes escaped
shQuoteEsc <- function(x) {
  out <- gsub("\'","\\\\'",x)
  out <- paste("'",out,"'",sep="")
  return(out)
}


##' replace ' with \\'
##' Also can replace ' with &143; type thingy
escapeQuotes <- function(x) UseMethod("escapeQuotes")
escapeQuotes.default <- function(x) x
escapeQuotes.character <- function(x) {
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
