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

## Common functions. Many of these just not used -- clean up.

## easy to find message
XXX <- function(...) message(..., "\n")

## merge two lists
merge.list <- function(x,y, overwrite=TRUE) {
  for(i in names(y)) {
    if(is.null(x[[i]]) || overwrite)
      x[[i]] <- y[[i]]
  }
  x
}

getWithDefault <- function(x, default=NULL) {
  if(is.null(x))
    return(default)
  if(!is.list(x) || !is.vector(x))
    return(x)
  if(is.na(x) || length(x) == 0)
    default
  else
    x
}

## this is how we handler coerce.with (gedit, gcombobox, gradio, ...)
do.coerce <- function(val, coerce.with) {
  if(is.null(val))
    return(val)
  ## do we coerce return value?
  if(is.null(coerce.with))
    return(val)
  else if(is.function(coerce.with))
    return(coerce.with(val))
  else if(is.character(coerce.with))
    return(do.call(coerce.with,list(val)))
  else
    return(val)               # what else?
}

#Paste = function(x,...) paste(x,...,sep="",collapse="")

## CONSTANTS
xyToAnchor <- function(anchor) {
  m <- rbind(
    c("nw","n","ne"),
    c("w","center","e"),
    c("sw","s","se")
    )
  anchor <- m[2 - anchor[2],2 + anchor[1]]
  return(anchor)
}

##' take xy anchor value and convert to alignment
##' @param anchor alignment specified as {-1,0,1}^2 value
xyToAlign <- function(anchor=NULL) {
  if(is.null(anchor))
    return(0L)
  
  halign <- list(Qt$Qt$AlignLeft, Qt$Qt$AlignHCenter, Qt$Qt$AlignRight)
  valign <- list(Qt$Qt$AlignBottom, Qt$Qt$AlignVCenter, Qt$Qt$AlignTop)
  align <- Qt$Qt$AlignCenter
  if(!is.null(anchor) && is.numeric(anchor) && length(anchor) >= 2) {
    align <- halign[[anchor[1] + 2]] | valign[[anchor[2] + 2]]
  }
  align
}


DEBUG <- function(...) {
  if(0)
    cat(paste(...,sep=" ",collapse=" "),"\n")
}

## paste() helpers
Paste = function(..., sep="", collapse="") {
  x = unlist(list(...))
  x = x[!is.na(x)]
  x = x[x != "NA"]
  paste(x, sep=sep, collapse=collapse)
}
PasteWithComma = function(...) {
  args = unlist(list(...))
  args = args[!is.na(args)]
  paste(args, sep="", collapse=", ")
}

## from regex man page
stripWhiteSpace = function(str) {
  sub('[[:space:]]+$', '', str) ## from ?gsub
  sub('^[[:space:]]+', '', str) ## from ?gsub
  return(str)
}


quoteIfNeeded <- function(str) {
  if(length(grep('^\\".*\\"$', str, perl=TRUE)) > 0 ||
     length(grep("^\\'.*\\'$", str, perl=TRUE)) > 0 )
    return(str)
  else
    return(paste('"',str,'"',sep="",collapse=""))
}

## ReadParseEvaL -- saves typing
rpel <- function(STRING, envir=.GlobalEnv) {
  eval(parse(text=STRING), envir=envir)
}

showErrorMessage <-  function() {       
  message = Paste("Error:",
    "\n\t",geterrmessage())
  gmessage(message,icon="error")
  stop()
}
  

## Push and Pop -- for convenience
Push <- function(v,d) c(v,d)
Pop <- function(v) ifelse(length(v) > 1, v[-length(v)], NA)


### is functions

is.guiWidget <- function(obj) {
  is(obj,"guiWidget")
}
is.gWidget <- function(obj) {
  is(obj,"gWidgetQt")
}

is.invalid <- function(obj) {
  stop("Implement me for Qt")
}
## used to check output 
is.empty <- function(obj) {
  if(is.null(obj) || is.na(obj) || obj == "") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


## for showing only possible values
is.dataframelike <- function(obj) {
  if(is.data.frame(obj) || is.matrix(obj) ||
     is.numeric(obj) || is.logical(obj) ||
     is.factor(obj)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## ## check if a gtkTreeViewCOlumn, make no GTK language
## is.gdataframecolumn = function(obj) {
##   if(class(obj)[1] == "GtkTreeViewColumn")
##     return(TRUE)
##   else
##     return(FALSE)
## }


### these are used by gvarbrowser
## This is from browseEnv in base
## what type of object is thixs and a size
str1 <- function(obj) {
  md <- mode(obj)
  lg <- length(obj)
  objdim <- dim(obj)
  if (length(objdim) == 0) 
    dim.field <- paste("length:", lg)
  else {
    dim.field <- "dim:"
    for (i in 1:length(objdim)) dim.field <- paste(dim.field, 
                                                   objdim[i])
    if (is.matrix(obj)) 
      md <- "matrix"
  }
  obj.class <- oldClass(obj)
  if (!is.null(obj.class)) {
    md <- obj.class[1]
    if (inherits(obj, "factor")) 
      dim.field <- paste("levels:", length(levels(obj)))
  }
  list( type = md, dim.field = dim.field)
}

## what type of object is thixs and a size
str2 <- function(obj) {
  md <- mode(obj)
  if (is.matrix(obj))  md <- "matrix"
  obj.class <- oldClass(obj)
  if (!is.null(obj.class)) {
    md <- obj.class[1]
  }
  return(md)
}

.datasets = c(
  "numeric","logical","factor","character",
  "data.frame","matrix","list",
  "table","xtabs",
  "nfnGroupedData","nffGroupedData","nmGroupedData"
  )
.models = c("lm","glm","lqs","aov","anova",
    "lme","lmList","gls",
  "ar","arma","arima0","fGARCH","fAPARCH"
    )
.ts = c("ts", "mts", "timeSeries", "its", "zoo")
.functions=c("function")
.plots = c("recordedplot")

knownTypes = list(
  "data sets and models"=c(.datasets, .models, .ts),
  "data sets"= .datasets,
  "model objects" = .models,
  "time series objects" = .ts,
  "functions"=.functions,
  "saved plots" = .plots,
  "all" = NULL
  )



## untaint a variable name so that $ can be used
untaintName <- function(objName) {
  if (length(grep(" |\\+|\\-|\\*|\\/\\(|\\[|\\:",objName)) > 0) {
    objName=Paste("\"",objName,"\"")
  }
  return(objName)
}

## try to stip off data frame stuff in fron to DND target
findDataParent <- function(x) {
  child = sub(".*]]","",x)
  child = sub(".*\\$","",child)
  parent = sub(Paste(child,"$"),"",x)
  parent = sub("\\$$","",parent)
  return(list(child=child,parent=parent))
}


## basically repeat findDataParent until no parent
findRootObject <- function(x) {
  x = sub("\\[\\[.*","",x)
  x = sub("\\$.*","", x)
  return(x)
}


## get does not work with name$component, this gets around that
## returns NULL if not available
getObjectFromString <- function(STRING="", envir=.GlobalEnv) {
  tmp = try(get(STRING, envir), silent = TRUE)
  if(!inherits(tmp, "try-error")) return(tmp)
  
  tmp = try(rpel(STRING,envir), silent=TRUE)
  if(!inherits(tmp, "try-error"))  return(tmp)

  ## out of chances
  return(NULL)
}



## get the names of the object, if available (datastores)
getNamesofObject <- function(STRING="") {
  ## if empty string, get variables in .GlobalEnv
  if(length(STRING) == 0 || STRING == "") {
    ## return objects of certain type
    objects = getObjectsWithType(root=NULL, filter=knownTypes[['data sets']])
    return(unlist(objects$Name))
  } 
  obj = getObjectFromString(STRING)
  if(!is.null(obj)) {
    if(is.list(obj)) {
      return(names(obj))
    } else if(is.matrix(obj)) {
      return(colnames(obj))
    } else{
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

## a function to get objects and their types
## filter is a vector of classes
getObjectsWithType <- function(root=NULL, filter = NULL, envir=.GlobalEnv) {

  if(is.null(root)) {
    objects = ls(envir=envir)
  } else {
    STRING = Paste("with(",root,",ls())")
    objects = try(rpel(STRING,envir=envir), silent=TRUE)
  }

  ## if empty send back
  if(length(objects) == 0)
    return( data.frame(Name=c(""),Type=c(""), stringsAsFactors=FALSE))

  ## proceed
  
  ## objects is character vector of components of root.
  badnames = grep("[[<-]|\\*",objects)
  if(length(badnames) > 0)
    objects = objects[-badnames]

  objectsWithRoot = sapply(objects,function(i) makeObjectName(root,i))

  
  type = sapply(objectsWithRoot, function(i) {
    STRING = Paste("str2(",i,")")
    rpel(STRING, envir=envir)
  })

  objects = data.frame(Name=objects,Type=type, stringsAsFactors=FALSE)

  ## filter
  if(!is.null(filter))
    objects = objects[type %in% filter,]
  
  return(objects)
}


## Find the name of the object by pasting toghther the pieces
## better to do name$name, but value may be a numeric
makeObjectName <- function(root,value) {
  if(is.null(root)) return(untaintName(value))

  ## now decide between $ and [[]]
  if(value == make.names(value)) {
    return(Paste(root,"$",untaintName(value)))
  } else {
    return(Paste(root,"[['",value,"']]"))
  }
}



######
## send a file to csv mode for editing
"browseDataAsCSV" <-
  function(x) {

    x = try(as.data.frame(x))
    if(inherits(x,"try-error")) {
      stop("Can not coerce data into a data frame")
    }

    tmpfile = paste(tempfile(),".csv",sep="",collapse="")
    write.csv(x,file=tmpfile)
    browseURL(paste("file://",tmpfile,sep="",collapse=""))

  }

## help out with gtree
byReturnVector <- function(df, FUN,...) {
  tmp = by(df, factor(1:nrow(df)), FUN)
  sapply(tmp, function(x) x)
}

hack.as.data.frame <- function(items) {
  ## check rectangular, or coerce to rectangules
  if(!(is.data.frame(items) || is.matrix(items) || is.vector(items))) {
    warning("Needs rectangular data, either a vector, matrix or data.frame")
    return(NA)
  }
  
  ## coerce to data frame
  if(is.vector(items)) {
    itemsName = deparse(substitute(items))
    items = data.frame(I(items))
    names(items) = itemsName
  }
  if(is.matrix(items)) {
    items = hack.as.data.frame.matrix(items) # fun in common.R
  }
  return(items)
}

## no easy way to not convert character vectors. This is a hack.
hack.as.data.frame.matrix <- 
  function (x, row.names = NULL, optional = FALSE) 
  {
    d <- dim(x)
    nrows <- d[1]
    ir <- seq(length = nrows)
        ncols <- d[2]
    ic <- seq(length = ncols)
    dn <- dimnames(x)
    if (missing(row.names)) 
      row.names <- dn[[1]]
    collabs <- dn[[2]]
    if (any(empty <- nchar(collabs) == 0)) 
      collabs[empty] <- paste("V", ic, sep = "")[empty]
    value <- vector("list", ncols)
    if (mode(x) == "character") {
      for (i in ic) value[[i]] <- as.character(x[, i])
    }
    else {
      for (i in ic) value[[i]] <- as.vector(x[, i])
    }
    if (length(row.names) != nrows) 
      row.names <- if (optional) 
        character(nrows)
      else as.character(ir)
    if (length(collabs) == ncols) 
      names(value) <- collabs
    else if (!optional) 
      names(value) <- paste("V", ic, sep = "")
        attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
  }

##################################################
## timestamp function for objects made with pmg
## Modified from R mailing list, value is comment. Need <- to act in
## OO manner comment needs to be a character vector. If a list were
## okay (say serialize()) then this could be different
"Timestamp<-" <- function(obj,value) {
  currentStamp = Timestamp(obj)
  currentStamp = c(currentStamp, timestamp=as.character(Sys.time()),comment=value)
  comment(obj) <- currentStamp
  return(obj)
}

Timestamp <- function(obj,k=1) {
  currentComment= comment(obj)
  allStamps =comment(obj)[names(comment(obj)) %in% "timestamp"]
  n = length(allStamps)
  if(n > 0)
    return(allStamps[(max(1,n+1-k)):n])
  else
    return(NA)
}

