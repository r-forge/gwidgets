library(gWidgetsWWW)

w <- gwindow("Simple GUI for R using gWidgetsWWW")

## load exampels
ex_files <- list.files(path=system.file("Examples", package="gWidgetsWWW"), pattern="^ex-")
Examples <- lapply(ex_files, function(i) gaction(i, tooltip=sprintf("Show example %s", i), icon="example",
                                                 parent=w,
                                                 handler=function(...) localServerOpen(sprintf("Examples/%s",i), package="gWidgetsWWW")))
names(Examples) <- gsub("\\.R$","",ex_files)

## gaction items need a parent
menuList <- list(File=list(
                   browseWorkspace=gaction("Browse workspace...",icon="browse", parent=w,
                     handler=function(h,...) summarizeWorkspaceDialog()),
                   loadPackage=gaction("Load Package...", parent=w, handler=function(h,...) {
                     loadPackageDialog()
                   }), 
                   loadDataset=gaction("Load Data Set...", parent=w, handler=function(h,...) {
                     loadDataSetDialog()
                   }), 
                   listDataFrame=gaction("List Data Frames...", parent=w, handler=function(h,...) {
                     listDataFramesDialog()
                   })
                   ),
                 Help=list(
                   help=gaction("Help", icon="help", parent=w, handler=function(h,...) helpDialog()),
                   Examples=Examples,
                   about=gaction("About", icon="about", parent=w, handler=function(h,.. ) aboutDialog())
                   )
                 )
                 


gmenu(menuList, cont=w)

## simple two pane layout
## a commandline and frame
g <- ggroup(cont=w)
gcommandline(cont=g,  graphic_size=c(800,500))


##################################################
## Some dialogs

## workspace browser, can extend by adding more buttons
summarizeWorkspaceDialog <- function() {
  summarizeWorkspace <- function(envir=.GlobalEnv) {
    as.data.frame(do.call("rbind", lapply(ls(envir=envir), function(i) c(name=i, class=class(get(i))[1]))),
                  stringsAsFactors=FALSE)
  }
out <- summarizeWorkspace()

w1 <- gwindow("Workspace objects",parent=w, width=500, height=600)
#  w1 <- gwindow("Workspace objects",  width=500, height=600)
  workspaceGroup <- ggroup(cont=w1, horizontal=FALSE)
  bg <- ggroup(cont=workspaceGroup)

buttons <- list()
buttons$refresh <- gbutton("refresh", cont=bg)
addSpace(bg, 16)

buttons$str <- gbutton(" str() ", cont=bg)
buttons$rm <- gbutton(" rm() ", cont=bg)

sapply(buttons[-1], function(i) enabled(i) <- FALSE) # not refresh


glabel("Workspace objects:", cont=workspaceGroup)
workspaceTable <- gtable(out, cont=workspaceGroup)
size(workspaceTable) <- c(600,400)

g2 <- ggroup(cont=workspaceGroup, horizontal=TRUE)
addSpace(g2, 10)
glabel("Filter by class:", cont=g2)
workspaceClasses <- gcombobox(c("<All>",unique(summarizeWorkspace()$class)), cont=g2)

gseparator(cont=workspaceGroup)
gbutton("dismiss", cont=workspaceGroup, handler=function(h,..) dispose(w1))

  ## handlers
handlers <- list()
handlers$refresh <- function(...) {
    out <- summarizeWorkspace()
    workspaceTable[] <- out
    workspaceClasses[] <- c("<All>", unique(out$class))
    filterHandler()
  }
handlers$str <- function(...) {
  val <- svalue(workspaceTable)
  if(val != "") {
    galert(val)
    obj <- get(val, envir=.GlobalEnv)
    out <- capture.output(str(obj))
    out <- sprintf("<code>%s</code>", paste(out, collapse="<br>"))
    w2 <- gwindow(sprintf("Calls str on %s", val), parent=w1)
    g <- ggroup(cont=w2, horizontal=FALSE)
    ghtml(out, cont=g)
    gseparator(cont=g)
    gbutton("dismiss", cont=g, handler=function(h,...) dispose(w2))
    visible(w2) <- TRUE
  } else {
    galert("Select a value")
  }
}

handlers$rm <- function(...) {
  val <- svalue(workspaceTable)
  if(val != "") {
    rm(list=val, envir=.GlobalEnv)
    galert(sprintf("Removed %s from workspace", val))
    handlers$refresh()
    sapply(buttons[-1], function(i) enabled(i) <- FALSE)
  } else {
    galert("Select a value")
  }
}

filterHandler <- function(...) {
  val <- svalue(workspaceClasses)
  if(val == "<All>")
    val <- ""
  workspaceTable$filter("class", val)    # clears if ""
  ## should change buttons if no selection, but punt here
}  

sapply(names(buttons), function(i) addHandlerClicked(buttons[[i]], handler=handlers[[i]]))
addHandlerChanged(workspaceClasses, handler=filterHandler)
addHandlerClicked(workspaceTable, handler=function(h,...) {
  x <- svalue(h$obj, index=TRUE)
  sapply(buttons[-1], function(i) enabled(i) <- (length(x) > 0))
})

visible(w1) <- TRUE

}
  

## load a package
loadPackageDialog <- function() {
  installed <- installed.packages()[,1]
  out <- sessionInfo()
  loaded <- c(out$basePkgs, names(out$otherPkgs))
  out <- data.frame(Available=installed, Loaded=installed %in% loaded, stringsAsFactors=FALSE)

  w1 <- gwindow("Load a package", parent=w, width=500, height=500)
  g <- ggroup(cont=w1, horizontal=FALSE)
  glabel("Double click to load package:", cont=g)
  tbl <- gtable(out, cont=g); size(tbl) <- c(400, 400)
  addHandlerDoubleclick(tbl, handler=function(h,...) {
    i <- svalue(h$obj)
    galert(sprintf("Loading package %s", i), delay=1)
    do.call("require", list(i))
    dispose(w1)
  })
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
}
          
loadDataSetDialog <- function() {
  out <- as.data.frame(data()$results[,-2], stringsAsFactors=FALSE)

  w1 <- gwindow("Load a data set", parent=w, width=500, height=500)
  g <- ggroup(cont=w1, horizontal=FALSE)
  glabel("Double click to load a built in data set:", cont=g)
  tbl <- gtable(out, cont=g); size(tbl) <- c(400, 400)
  addHandlerDoubleclick(tbl, handler=function(h,...) {
    i <- svalue(h$obj, index=TRUE)
    galert(sprintf("load data %s in package %s", out[i,2], out[i,1]), delay=1)
    do.call("data", list(out[i,2], package=out[i,1]))
    dispose(w1)
  })
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
}

listDataFramesDialog <- function() {
  x <- ls(envir=.GlobalEnv)
  if(length(x))
    out <- x[sapply(x, function(i) is.data.frame(get(i, envir=.GlobalEnv)))]
  else
    out <- c()


  w1 <- gwindow("Available data frames", parent=w, width=500, height=500)
  g <- ggroup(cont=w1, horizontal=FALSE)
  if(length(out)) {
    size <- sapply(out, function(i) {
      obj <- get(i, envir=.GlobalEnv)
      sprintf("%s rows by %s columns", dim(obj)[1], dim(obj)[2])
    })
              
    out <- data.frame(Datasets=out, size=size, stringsAsFactors=FALSE)
    tbl <- gtable(out, cont=g); size(tbl) <- c(400, 400)
    addHandlerClicked(tbl, handler=function(h,...) {
      i <- svalue(tbl)
      w2 <- gwindow(sprintf("Detail on %s", i), parent=w1, width=600, height=600)
      g <- ggroup(cont=w2, horizontal=FALSE)
      tbl2 <- gtable(get(i, envir=.GlobalEnv), cont=g)
      size(tbl2) <- c(550,500)
      gbutton("dismiss", cont=g, handler=function(...) dispose(w2))
      visible(w2) <- TRUE
    })
  } else {
    glabel("No available data frames", cont=g)
  }
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
}

## help dialog
helpDialog <- function() {
  ## get help from string
  getHelpOptions <- function(str) {
    blank <- data.frame(topic="", package="", description="", stringsAsFactors=FALSE)
    if(str=="") {
      out <- blank
    } else {
      tmp <- help.search(str, fields="name")$matches
      if(nrow(tmp) == 0) {
        out <- blank
      } else if(nrow(tmp) > 50) {
        out <- data.frame(topic="", package="", description="Too many matches", stringsAsFactors=FALSE)
      } else {
        tmp[,2] <- gsub("\\n"," ", tmp[,2])
        out <- data.frame(topic=tmp[,1],
                          package=tmp[,3],
                          description=tmp[,2],
                          stringsAsFactors=FALSE)
      }
    }
    out
  }
  

  w1 <- gwindow("Search help pages", parent=w, width=500, height=625)
  g <- ggroup(cont=w1, horizontal=FALSE)
  g1 <- ggroup(cont=g)
  glabel("Enter a string to match:", cont=g1)
  e <- gedit("", cont=g1)
  b <- gbutton("search", cont=g1)
  
  tbl <- gtable(getHelpOptions(""), cont=g)
  size(tbl) <- c(500,500)
  addHandlerClicked(b, handler=function(h, ...) {
    val <- svalue(e)
    tbl[] <- getHelpOptions(val)
  })
  
  addHandlerClicked(tbl, function(h,...) {
    ind <- svalue(tbl, index=TRUE)
    topic <- tbl[ind,1]
    package <- tbl[ind,2]
    w2 <- gwindow(sprintf("Help on %s", topic), parent=w1, width=800, height=650)
    g <- ggroup(cont=w2, horizontal=FALSE)
    ghtml(sprintf("http://127.0.0.1:%s/library/%s/html/%s.html",
                  tools:::httpdPort,
                  package, topic), cont=g)
    gseparator(cont=g)
    gbutton("dismiss", cont=g, handler=function(h,..) dispose(w2))
    visible(w2) <- TRUE
  })
  gbutton("dismiss", cont=g, handler=function(h,..) dispose(w1))
  visible(w1) <- TRUE
}

## about dialg
aboutDialog <- function() {
  w1 <- gwindow("About", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  ghtml(paste("<h2>A simple GUI</h2>",
              "This demo shows a simple GUI for R to be run through",
              "the local web server provided by R for its help pages.",
              "The idea is to show how the widgets can be combined to produce",
              "some familiar looking dialogs.",
              "The main text boxes are for issuing commands. When a command is typed,",
              "either clicking 'Evaluate' or the ENTER key will being the commands evaluation.",
              sep=" "),
        cont=g)
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
}

## branding
gstatusbar("Powered by gWidgetsWWW", cont=w)
visible(w) <- TRUE
