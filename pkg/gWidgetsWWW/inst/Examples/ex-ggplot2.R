## GUI for exploring a data set using ggplot2
## Needs some -- alot of -- work! better representation,  DRY is violated, passing parameters to
## ggplot2 commands, ...

if(!exists("gWidgetsWWWStaticDir") || is.null(gWidgetsWWWStaticDir))
  gWidgetsWWWStaticDir <- tempdir()

GUIdescription <- paste("An example GUI for exploring ggplot2 commands that illustrates:",
                        "the gcombobox with icons and tooltips,",
                        "dynamically adding widgets;",
                        "the gexpandgroup container for space management;",
                        "and gsvg for displaying graphics.",
                        "Not all the ggplot commands work, including some that should.",
                        "This seems to be an issue with the device driver not being happy with",
                        "certain labels.",
                        sep=" ")


## we want to quiet down the loading of packages
deleteMe <- tempfile()
sink(deleteMe)
require(reshape, quietly=TRUE, warn=FALSE)
require(plyr, quietly=TRUE, warn=FALSE)
require(grid, quietly=TRUE, warn=FALSE)
require(ggplot2, quietly=TRUE, warn=FALSE)
sink()
unlink(deleteMe)


##################################################
## gpplot setup
## Get Geoms, Stat, Scale, Coord
types <- c("Geom", "Stat","Scale","Coord")

## function to convert GeomBoxplot <-> geom_boxplot
upperToLower <- function(x) {
  for(i in types) {
    if(length(grep(paste("^",i, sep=""), x))) {
      x <- gsub(paste("^",i, sep=""),"",x)
      return(paste(tolower(i),tolower(x), sep="_"))
    }
  }
  return(x)
}
lowerToUpper <- function(x) {
  .simpleCap <- function(x) {
         s <- strsplit(x, " ")[[1]]
         paste(toupper(substring(s, 1,1)), substring(s, 2),
               sep="", collapse=" ")
     }
  tmp <- unlist(strsplit(x,"_"))
  if(length(tmp) > 1)
    return(paste(.simpleCap(tmp[1]),.simpleCap(tmp[2]), sep=""))
  else
    return(tmp)
}

Units <- list()
for(i in types)
  Units[[i]] <- apropos(paste("^",i, sep=""), ignore.case=FALSE)[-1]
## scales are done differently
Units$scales <- apropos("^scale_x",ignore.case=FALSE)
Units$Scales <- lowerToUpper(gsub("_x","",Units$scales))


## make icons if not there
iconFiles <- list()
for(type in types) {
  iconFiles[[type]] <- Units[[type]]
  for(i in seq_along(Units[[type]])) {
    f <- paste(gWidgetsWWWStaticDir,.Platform$file.sep, Units[[type]][i],".png", sep="")
    if(!file.exists(f)) {
      obj <- get(Units[[type]][i])
      png(f, width=16, height=16)
      grid.newpage()
      out <- try(grid.draw(obj$icon()), silent=TRUE)
      dev.off()
      if(inherits(out,"try-error"))
        f <- ""
    }
    iconFiles[[type]][i] <- f
  }
  addStockIcons(Units[[type]], convertStaticFileToUrl(iconFiles[[type]]))
}


##################################################
## our data set (was baseball from plyr, but too large for an example)

data("Cars93", package="MASS")
bb <- Cars93

w <- gwindow("ggplot2 GUI")
g <- ggroup(cont = w, horizontal=FALSE)
g1 <- ggroup(cont=g, width=700)
ghtml(GUIdescription, cont = g1)
gseparator(cont = g)

hg <- ggroup(horizontal=TRUE, cont = g)
lg <- ggroup(horizontal=FALSE, cont = hg, width=450)
rg <- ggroup(horizontal=FALSE, cont = hg, width=600)

width <- 580; height <- 500
devs <- list()

imageFile <- getStaticTmpFile(ext=".svg")
imageDevice <-  gsvg(cont = rg,  expand=TRUE, width=width, height=height)

## for ggplot we have commands such as
glabel("p <- ggplot(Cars93) + ", cont = lg)


##################################################
## aesthetics, should be more here


f <- gexpandgroup("aes: specify x and y aesthetics.", cont = lg)
aesWidgets <- list()
tbl <- glayout(cont = f)
tbl[1,1] <- "x"
tbl[1,2] <- (aesWidgets[["x"]] <- gcombobox(c("",names(bb)), editable=TRUE, cont = tbl))

tbl[2,1] <- "y"
tbl[2,2] <- (aesWidgets[["y"]] <- gcombobox(c("",names(bb)), editable=TRUE, cont = tbl))



## Geoms
f <- gexpandgroup("Geoms: specify geometric object(s) to plot", cont = lg)

GeomCb <- list()
GeomDf <- data.frame(values = c("", upperToLower(Units$Geom)),
                 icon = c("GeomBlank", Units$Geom),
                 qtip = c("", sapply(Units$Geom, function(i) {
                   obj <- getFromNamespace(i, ns="ggplot2")
                   obj$desc
                 })),
                 stringsAsFactors=FALSE)

makeGeomSelector <- function(f) {
  g1 <- ggroup(cont = f)
  glabel("+ ", cont = g1)
  n <- length(GeomCb)
  GeomCb[[n+1]] <<- list()
  GeomCb[[n+1]]$widget <<- gcombobox(GeomDf, cont = g1,  handler = function(h,...) {
    out <- sapply(GeomCb, function(i) svalue(i$widget))
    if(!any(sapply(out, function(i) i == "")))
      makeGeomSelector(f)
  })
  GeomCb[[n+1]]$aes <<- list()
  gbutton("aes", cont = g1, action = n + 1, handler = function(h,...) {
    n <- h$action
    val <- svalue(GeomCb[[n]]$widget)
    val <- lowerToUpper(val)
     if(val != "") {
       pobj <- getFromNamespace(val, ns="ggplot2")
       needThese <- setdiff(pobj$required_aes, c("x","y"))
       if(is.null(needThese) || length(needThese) == 0) {
         gmessage("Nothing to configure", parent=w) ## galert gets buried
         return()
       } else {
         val <- paste(needThese, collapse=" ")
       }
     }
    
    w1 <- gwindow("Edit aes options", parent=w)
    g <- ggroup(cont = w1, horizontal=FALSE)
    glabel("This is not working!", cont = g)
    tbl <- glayout(cont = g)
    lst <- list()
    for(i in seq_along(needThese)) {
      val <- needThese[i]
      tbl[i,1] <- val
      tbl[i,2] <- (lst[[val]] <- gedit("", cont = tbl))
      if(!is.null(tmp <- GeomCb[[n]]$aes[[val]]) && nchar(as.character(tmp))) { # update if present
        svalue(lst[[val]]) <- tmp
      }
    }
    gseparator(cont = g)
    g1 <- ggroup(cont = g)
    gbutton("dismiss", cont = g1, handler = function(h,...) dispose(w1))
    gbutton("ok", cont = g1, handler=function(h,...) {
      vals <- lapply(lst, svalue)
      GeomCb[[n]]$aes <<- vals
      dispose(w1)
    })
    visible(w1) <- TRUE
  })
}
makeGeomSelector(f)

##################################################
## Stats

f <- gexpandgroup("Stats: specify data transformations", cont = lg); visible(f) <- FALSE

StatCb <- list()
StatDf <- data.frame(values = c("", upperToLower(Units$Stat)),
                 icon = c("GeomBlank", Units$Stat),
                 qtip = c("", sapply(Units$Stat, function(i) {
                   obj <- getFromNamespace(i, ns="ggplot2")
                   obj$desc
                 })),
                 stringsAsFactors=FALSE)

makeStatSelector <- function(f) {
  g1 <- ggroup(cont = f)
  glabel("+ ", cont = g1)
  n <- length(StatCb)
  StatCb[[n+1]] <<- list()
  StatCb[[n+1]]$widget <<- gcombobox(StatDf, cont = g1,  handler = function(h,...) {
    out <- sapply(StatCb, function(i) svalue(i$widget))
    if(!any(sapply(out, function(i) i == "")))
      makeStatSelector(f)
  })
}
makeStatSelector(f)

##################################################
## Coord

f <- gexpandgroup("Coords: adjust coordinate mappings", cont = lg); visible(f) <- FALSE

CoordCb <- list()
CoordDf <- data.frame(values = c("", upperToLower(Units$Coord)),
                 icon = c("GeomBlank", Units$Coord),
                 qtip = c("", sapply(Units$Coord, function(i) {
                   obj <- getFromNamespace(i, ns="ggplot2")
                   obj$desc
                 })),
                 stringsAsFactors=FALSE)

makeCoordSelector <- function(f) {
  g1 <- ggroup(cont = f)
  glabel("+ ", cont = g1)
  n <- length(CoordCb)
  CoordCb[[n+1]] <<- list()
  CoordCb[[n+1]]$widget <<- gcombobox(CoordDf, cont = g1,  handler = function(h,...) {
    out <- sapply(CoordCb, function(i) svalue(i$widget))
    if(!any(sapply(out, function(i) i == "")))
      makeCoordSelector(f)
  })
  CoordCb[[n+1]]$aes <<- 1
}
makeCoordSelector(f)

##################################################
## Scale

f <- gexpandgroup("Scales: control mapping between data and aesthetics", cont = lg); visible(f) <- FALSE

## ScaleDf is different as x and y are needed
ScaleCb <- list()
ScaleDf <- matrix(character(2*length(Units$scales)*3 + 3), ncol=3)
ScaleDf[1,] <- c("","GeomBlank","Empty value")
for(i in seq_along(Units$scales)) {
  ScaleDf[2*i,1] <- Units$scales[i]
  ScaleDf[2*i + 1  ,1] <- gsub("_x","_y",Units$scales[i])
  ScaleDf[(2*i):(2*i+1), 2] <- Units$Scale[i]
  ScaleDf[(2*i):(2*i+1), 3] <- getFromNamespace(Units$Scale[i], ns="ggplot2")$desc
}
ScaleDf <- as.data.frame(ScaleDf, stringsAsFactors=FALSE)

makeScaleSelector <- function(f) {
  g1 <- ggroup(cont = f)
  glabel("+ ", cont = g1)
  n <- length(ScaleCb)
  ScaleCb[[n+1]] <<- list()
  ScaleCb[[n+1]]$widget <<- gcombobox(ScaleDf, cont = g1,  editable=TRUE, handler = function(h,...) {
    out <- sapply(ScaleCb, function(i) svalue(i$widget))
    if(!any(sapply(out, function(i) i == "")) && length(ScaleCb) < 2)
      makeScaleSelector(f)
  })
  ScaleCb[[n+1]]$aes <<- 1
}
makeScaleSelector(f)


gseparator(cont = lg)
b <- gbutton("Make plot", cont = lg, handler = function(h,...) makePlots())

## clean up
rm("bb")                                # dont' store in session
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE



makePlots <- function() {
  sink("/tmp/sink.R")
  require(reshape, quietly=TRUE, warn=FALSE)
  require(plyr, quietly=TRUE, warn=FALSE)
  require(grid, quietly=TRUE, warn=FALSE)
  require(ggplot2, quietly=TRUE, warn=FALSE)
  sink()

data("Cars93", package="MASS")
bb <- Cars93

  l <- list()
  for(i in names(aesWidgets)) {
    val <- svalue(aesWidgets[[i]])
    if(val != "")
      l[[i]] <- val
  }
  p <- ggplot(bb) +
    do.call("aes_string",l)
  
  geoms <- Units$Geom
  for(i in GeomCb) {
    val <- svalue(i$widget)
    if(is.character(val)) {
      type <- geoms[lowerToUpper(val) == geoms]         # check that we match
      if(length(type)) {
        l <- list()
        if(length(i$aes)) {
          lst <- lapply(i$aes, function(j) {
            if(is.na(j) || is.null(j) || nchar(as.character(j))== 0 || !is.character(j))
              NULL
            else
              i
          })
          if(length(lst)) {
            ## XXX This isn't working -- FIX ME
            l$aes <- do.call("aes_string", lst[-1])
          }
        }
        p <- p + do.call(val, l)    
      }
    }
  }
  ## Stats
  stats <- Units$Stat
  for(i in StatCb) {
    val <- svalue(i$widget)
    if(is.character(val)) {
      type <- stats[lowerToUpper(val) == stats]         # check that we match
      if(length(type)) {
        p <- p + do.call(val, list())     # do aes
      }
    }
  }
  ## Coord
    coords <- Units$Coord
  for(i in CoordCb) {
    val <- svalue(i$widget)
    if(is.character(val)) {
      type <- coords[lowerToUpper(val) == coords]         # check that we match
      if(length(type)) {
        p <- p + do.call(val, list())     # do aes
      }
    }
  }
  ## Scales
  scales <- Units$scale
  scales <- c(scales, gsub("_x","_y",scales))
  for(i in ScaleCb) {
    val <- svalue(i$widget)
    if(is.character(val)) {
      type <- scales[val == scales]         # check that we match
      if(length(type)) {
        p <- p + do.call(val, list())     # do aes
      }
    }
  }

  ## make images
  require(RSVGTipsDevice, quietly=TRUE, warn=FALSE)
  if(file.exists(imageFile))
    unlink(imageFile)                     # out with the old ...
  imagefile <- getStaticTmpFile(ext=".svg")
#  imagefile <- gsub("//","/",getStaticTmpFile(ext=".svg")) # in with the new
  devSVGTips(imageFile)
  out <- try(print(p), silent=TRUE)
  dev.off()

  if(inherits(out, "try-error")) {
    gmessage(out, parent=w)
  } else {
    ## update graphic    
    svalue(imageDevice) <- convertStaticFileToUrl(imageFile)
  }
  
}
