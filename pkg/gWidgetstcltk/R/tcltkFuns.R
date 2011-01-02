##' Coerce tclObj object to logical value
##'
##' @param x should be a "0" or "1" value
##' @param ... ignored
##' @return a logical or NA
as.logical.tclObj <- function(x, ...) as.logical(as.numeric(x))


##' Does object exists as tcl variable
##'
##' @param x character string with name of variable
##' @return logical
tclObj_exists <- function(x) as.logical(.Tcl(sprintf("info exists %s", x)))

##' create a tcl image from the file
##'
##' @param basename basename of image. We add some bit to avoid filename collisions
##' @param file file of image
##' @return image name
make_tcl_image <- function(basename, file) {
  already_defined <- function(nm) any(nm == as.character(tcl("image","names")))
  
  nm <- sprintf("::tcl::%s", basename)
  if(!already_defined(nm)) {
    tcl("image","create","photo", nm ,file=file)
  }
  return(nm)
}

##' Heuristic to determine if widget is a ttk widget
##'
##' @param x tk object or its id
##' @return logical indicating is  ttk widget or not
isTtkWidget <- function(x) {
  cl <- as.character(tkwinfo("class",x))
  grepl("^[A-Z]{2,}", cl)
}

##' what windowing system?
##'
##' @return one of c("x11", "win32", "aqua")
windowingsystem <- function() {
  ## one of x11 (X11-based), win32 (MS Windows), or aqua (Mac OS X Aqu
  as.character(.Tcl("tk windowingsystem"))
}

## return tk widget from obj
## ** This should be a method **
getWidget = function(obj) {
  if(is(obj,"tkwin")) return(obj)

  if(is(obj,"gWidgettcltk"))
    return(getWidget(obj@widget))
  else if(is(obj,"guiWidget"))
    return(getWidget(obj@widget))
  else
    return(NA)
}

getBlock = function(obj) {

  if(is(obj,"tkwin")) return(obj)
  if(is(obj,"gWidgettcltk"))
    return(getBlock(obj@block))
  else if(is(obj,"guiWidget"))
    return(getBlock(obj@widget))
  else
    return(NA)
}


getTopParent = function(tkobj) {
  ## in env is parent variable if present
  ans <- NULL
  
  while(is.null(ans)) {
    e <- tkobj$env$parent
    if(is.list(e)  &&
       e[['ID']] =="")
      ans <- tkobj
    else tkobj <- tkobj$env$parent
  }
  return(ans)
}

getTopLevel <- function(obj) {
  if(is(obj, "guiWidget")) {
    return(getTopLevel(obj@widget))
  } else if(!is.null(obj@e$parentContainer)) {
    return(getTopLevel(obj@e$parentContainer)) 
  } else {
    return(obj)
  }
}

setMethod(".getToolkitWidget",
          signature(obj="gWidgettcltk", toolkit="guiWidgetsToolkittcltk"),
          function(obj, toolkit) getWidget(obj))




## Does the top level window exists
windowExists = function(obj) {
  win = getTopParent(getWidget(obj))
  as.logical(tkwinfo("exists", win))
}

findTkIcon <- function(i) {
  if(is.null(i)) return("")
  
  stock <- getStockIcons()
  gwi <- system.file(paste("image/",i,".png",sep=""), package="gWidgetstcltk")
  if(is.null(i) || is.na(i) || i == "")
    val <- ""
  else if(file.exists(i))
    val <- i
  else if (file.exists(gwi))
    val <- gwi
  else
    val <- stock[[i, exact=TRUE]]
  ## what to return
  if(is.null(val))
    return("")
  else
    return(val)
}


##################################################
## function to add scrollbars to widget and pack into grid
addScrollbarsToWidget <- function(widget, parent) {
  xscr <- ttkscrollbar(parent, orient="horizontal",
                       command=function(...) tkxview(widget, ...))
  yscr <- ttkscrollbar(parent, orient="vertical",
                       command=function(...) tkxview(widget, ...))

  tkconfigure(widget,
              xscrollcommand=function(...) tkset(xscr,...),
              yscrollcommand=function(...) tkset(yscr,...))

  tkgrid(widget, row=0, column=0, sticky="news")
  tkgrid(yscr,row=0,column=1, sticky="ns")
  tkgrid(xscr, row=1, column=0, sticky="ew")
  tkgrid.columnconfigure(parent, 0, weight=1)
  tkgrid.rowconfigure(parent, 0, weight=1)
}


