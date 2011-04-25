library(gWidgets)
options(guiToolkit="Qt")

if(gWidgets:::.bypassRequire("testthat")) {


offspring <- function(path, user.data=NULL) {
  sep <- .Platform$file.sep
  if(length(path) > 0) {
    ext <- paste(path, collapse=sep)
    directory <- paste(getwd(), ext,  sep=sep)
  } else {
    directory <- getwd()
  }
  files <- file.info(dir(path=directory))[,c(2,1,3)]
  
  files <- cbind(rownames(files), files)
  names(files)[1] <- "filename"
  return(files)
}
hasOffspring <- function(children,user.data=NULL, ...) {
  return(children$isdir)
}

icon.FUN <- function(children,user.data=NULL, ...) {
  x <- rep("file", length=nrow(children))
  x[children$isdir] <- "directory"
  return(x)
  }


## shows isdir directory, as hasOffspring is specified
w <- gwindow("test with isdir showing")
tr <- gtree(offspring, hasOffspring, icon.FUN = icon.FUN, container=w)


## size
size(tr) <- c(400,400)

## svalue<-
svalue(tr) <- c(6,2)

## svalue
svalue(tr, drop=TRUE, index=FALSE)       # default -- gives key
svalue(tr, drop=TRUE, index=TRUE)         # gives last index (useless)?
svalue(tr, drop=FALSE, index=FALSE)       # gives whole path
svalue(tr, drop=FALSE, index=TRUE)        # gives index of path


## [



## handlers
sapply(c("Changed", "Clicked", "Doubleclick"),
       function(i) {
         f <- function(h,...) print(h$action)
         do.call(sprintf("addHandler%s", i), list(obj=tr, handler=f, action=i))
       })
}
