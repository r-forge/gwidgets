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


## interface to Processing.js
## handlers of processingEvents return javascript using processing. Use <<p>> to refer to processing
## object. For instance
## p$mouseMoved <- function(.) {
##  paste("<<p>>.background(0)",
##        "<<p>>.line(1,2,4,5)
## }

##' Widget to allow low-level graphics commands through processingjs.
##'
##' The advantage over gcanvas is that this has more interactivity
##' defined for it.  The use of this is different though. The basic
##' idea is that several of the lowlevel plot commands are
##' implemented. For exmaple plot.new, plot.window, axis, title,
##' lines, polygon etc. These are called differently though. They are
##' implemented as proto methods of the gprocessingjs object, so are
##' called as in p\$plot.window().  The method addHandlerClick passes
##' back a value xy wich contains the position of the mouse click in
##' pixel coordinates. One can use the method pixelsToXY to covert to
##' usr coordinates.
##' @param width of graphic
##' @param height height of graphic (pixels)
##' @param pointsize size of fonts
##' @param container parent container
##' @param ... passed to container's add method
##' @export
gprocessingjs <- function(width=400, height=400, pointsize= 12, container = NULL, ...) {
  widget <- EXTComponentNoItems$new(toplevel = container$toplevel,
                             ..width = width,
                             ..height = height,
                             ..pointsize = pointsize)
  class(widget) <- c("gProcessingJS", class(widget))


  ## addHandlerMouseMove
  EXTWidget$addHandlerMousemove <- function(., handler, action=NULL) {
    .$addHandler(signal="mousemove",handler, action,
                 handlerArguments="e",
                 handlerExtraParameters = "Ext.util.JSON.encode({xy:[e.layerX,e.layerY]})"
                 )
    
  }


  
  ## properties
  widget$..margin <- as.integer(c(1,2,1.5,3) * pointsize * 1.2)
  widget$..background <- 250 ## passed to .$background via plot.new
  
  ## holds value as string
  widget$out <- String()
  

  widget$ExtConstructor <- "Ext.ux.Canvas"
  widget$ExtCfgOptions <- function(.) {
    out <- list()

    return(out)
  }


  widget$asProcessingCharacter <- function(.) sprintf("processing%s", .$ID)
  widget$..writeConstructor <- function(.) {
    ## create element
    out <- String() + "\n" + "// ------- \n" +
      sprintf("var %s = new Processing(document.getElementById('%s'));", .$asProcessingCharacter(), .$ID) +
        sprintf("%s.size(%s, %s);", .$asProcessingCharacter(), .$..width, .$..height) +
          .$out + "\n"
    out
  }
    
  widget$footer <- function(.) {
    ID <- .$ID
    pID <- .$asCharacter()

    out <- String()
    
    ## for i in handlers, call
    ## these are javascript to call direct avoiding R handlers.
    for(i in .$processingEvents) {
      f <- .[[i]]
      if(!is.null(f)) {
         val <- f()
         pID <- String("processing") + ID
         fnHead <- pID + "." + i + "= function() {\n"
         val <- gsub("<<p>>",pID, val) ## sub for processing ID
         val <- paste(val, collapse="\n")
         out <- out + fnHead + val + "};\n"
       }
     }
     ## call init
#    out <- out + sprintf("%s.init();", .$asCharacter())
    out
  }
  
  ## turn method into javascript command from Processing.js
  widget$makeCommand <- function(.,name, ...) {
    args <- list(...)
    
    vals <- paste(args, collapse=", ")

    val <-  String() +
      "processing" + .$ID +  "." + name + "(" + vals + ");\n"

    if(.$has_local_slot("..shown"))
      .$addJSQueue(val)
    else
      .$out <- .$out + val
  }

  ## if these are defined, then they are to be functions returning
  ## javascript code the interactive handlers can be **really slow**
  ## if sent back into R. Use gWidgets Handlers to put in interactive
  ## code with R commands. These allow interactive demos at the expense
  ## of being able to program in javascript The value <<p>> will
  ## expand to the appropriate object ID.
  ## 
  widget$processingEvents <- c("mouseDragged","mouseMoved", "mousePressed", "mouseReleased",
                               "keyPressed","keyReleased","draw","setup")
  
  for(i in widget$processingEvents) widget[[i]] <- NULL

  ## functions for mouse positions when making javascript handlers
  widget$pmouseX <- function(.) String("processing") + .$ID + ".pmouseX"
  widget$pmouseY <- function(.) String("processing") + .$ID + ".pmouseY"
  widget$mouseX <- function(.) String("processing") + .$ID + ".mouseX"
  widget$mouseY <- function(.) String("processing") + .$ID + ".mouseY"
  widget$mouseButton <- function(.) String("processing") + .$ID + ".mouseButton"

  ##################################################
  ## Make functions for processing commands
  ## Mostly copied over from Processing.js. Most aren't used
  ## ???
  widget$color <- function(., aValue1, aValue2, aValue3, aValue4)
    .$makeCommand("color", aValue1, aValue2, aValue3, aValue4)

  ## pad with leading 0's
  widget$nf <- function(., num, pad) .$makeCommand("nf", num, pad)

  ## ??
  widget$AniSprite <- function(., prefix, frames)
    .$makeCommand("AniSprite", prefix, frames)

  ## How would this work -- obj is a javascript object?
  widget$buildImageObject <- function(., obj)
    .$makeCommand("buildImageObject", obj)

  ## build image
  widget$createImage <- function(., w, h, mode)
    .$makeCommand("createImage", w, h, mode)

  ## stub -- handled through gWidgets
  widget$createGraphics <- function(., w, h) ""
  widget$beginDraw <- function(.) .$makeCommand("beginDraw")
  widget$endDraw <- function(.) .$makeCommand("endDraw")

  widget$loadImage <- function(.,file)
    .$makeCommand("loadImage", file)
  
  widget$loadFont <- function(., name)
    .$makeCommand("loadFont", shQuote(name))
  widget$textFont <- function(., name, size)
    .$makeCommand("textFont", shQuote(name), as.integer(size))
  widget$textSize <- function(., size)
    .$makeCommand("textSize", as.integer(size))
  widget$textAlign <- function(.)
    .$makeCommand("textAlign")
  ## ptext - -so text will be R function
  widget$ptext <- function(., str, x, y) 
    .$makeCommand("text", str, x, y)
  widget$char <- function(., key)
    .$makeCommand("char", key)
  widget$println <- function(.)
    .$makeCommand("println")
  
  widget$map <- function(., value, istart, istop, ostart, ostop )
    .$makeCommands("map", value, istart, istop, ostart, ostop )

  widget$Point <- function(., x, y)
    .$makeCommands("Point", x, y)

  widget$Random <- function(.)
    .$makeCommands("Random")

  widget$ArrayList <- function(., size, size2, size3)
    .$makeCommands("ArrayList",size, size2, size3)

  widget$colorMode <- function(., mode, range1, range2, range3, range4 ) 
    .$makeCommand("colorMode", mode, range1, range2, range3, range4 )

  widget$beginShape <- function(., type )
    .$makeCommand("beginShape",type)

  widget$endShape <- function(., close="true" )
    .$makeCommand("endShape",close)

  widget$vertex <- function(., x, y) #x2, y2, x3, y3 )
  .$makeCommand("vertex",  x, y) #, x2, y2, x3, y3 )

  widget$curveVertex <- function(., x, y, x2, y2 )
    .$makeCommand("curveVertex", x, y, x2, y2 )

  widget$curveTightness <- function(., tightness)
    .$makeCommand("curveTightness", tightness)

  widget$rectMode <- function(., aRectMode)
    .$makeCommand("rectMode", aRectMode)

  widget$imageMode <- function(.)
    .$makeCommand("imageMode")

  widget$ellipseMode <- function(., aEllipseMode)
    .$makeCommand("ellipsMode", aEllipseMode)
  
  ## skip math ones
  
  widget$translate <- function(., x, y)
    .$makeCommand("translate", x,y)
  
  widget$scale <- function(., x,y)
    .$makeCommand("scale", x, y)
  
  widget$rotate <- function(., aAngle)
    .$makeCommand("rotate", aAngle)
  
  widget$redraw <- function(.) 
    .$makeCommand("redraw")

 widget$loop <- function(.)
   .$makeCommand("loop")

 widget$frameRate <- function(., aRate)
   .$makeCommand("frameRate", aRate)

 ## set background image, or color (gray scale 0 to 256?)
 widget$background <- function(., img=0) ## img could be image?
   .$makeCommand("background", img)



  widget$size <- function(., aWidth, aHeight) 
    .$makeCommand("size", aWidth, aHeight)

  widget$noStroke <- function(.) .$makeCommand("noStroke")

  widget$noFill <- function(.) .$makeCommand("noFill")

  widget$smooth <- function(.) .$makeCommand("smooth")

  widget$noLoop <- function(.) .$makeCommand("noLoop")

  widget$fill <- function(.,...) .$makeCommand("fill",...)

  widget$stroke <- function(.,...) .$makeCommand("stroke",...)

  widget$strokeWeight <- function(., w) .$makeCommand("strokeWeight", w)
  
  widget$point <- function(., x, y) .$makeCommand("point", x, y)
  
  ## rename with p -- otherwise get is an issue
  widget$pget <- function(., x, y) .$makeCommand("get", x, y)
   widget$pset <- function(., x, y, obj) .$makeCommand("set", x, y, obj)

  widget$arc <- function(., x, y, width, height, start, stop )
    .$makeCommand("arc",x, y, width, height, start, stop )

  ## draw a line. Make R like c(x,y), c(x1,y1)
  widget$line <- function(., x1, x2, y1, y2) {
    if(length(x1) == 2) 
      .$makeCommand("line", x1[1], x2[1], x1[2], x2[2])
    else
      .$makeCommand("line", x1,x2,y1,y2)
  }

  ## draw Bezier curve
  ## if x1 length 4, assume y1 is and replace
  widget$bezier <- function(.,x1,y1,x2,y2,x3,y3,x4,y4) {
    if(length(x1) == 4) {
      x2 <- x1[2]; x3 <- x1[3]; x4 <- x1[4]; x1 <- x1[1]
      y2 <- y1[2]; y3 <- y1[3]; y4 <- y1[4]; y1 <- y1[1]
    }
    .$makeCommand("bezier", x1, y1, x2, y2, x3, y3, x4, y4)
  }

  widget$triangle <- function(.,  x1, y1, x2, y2, x3, y3 )
    .$makeCommand("triangle", x1, y1, x2, y2, x3, y3 )

  widget$quad <- function(., x1, y1, x2, y2, x3, y3, x4, y4 )
    .$makeCommand("quad", x1, y1, x2, y2, x3, y3, x4, y4 )

  ## rect -> prect
  widget$prect <- function(., x,y, width, height)
    .$makeCommand("rect",  x, y, width, height)

  ## draw ellipse or circle
  widget$ellipse <- function(., x, y, width=10, height=width)
    .$makeCommand("ellipse", x, y, width, height)


  ##################################################
  ## Familiar R methods for plot devices, using those above
  ## not exactly a device interface, but not hard to use either.

  ## Some helper functions
  ## The canvas uses pixels, R uses coordinates within xlim/ylim. This converts x to Pixels
  ## separate --but identical -- function for y to pixels
  ## Respects p$..margin <- c(left, top, right, bottom) in pixels
  widget$xToPixels <- function(., x) {
    xlim <- .$getXlim()
    margin <- .$..margin

    for(k in 2:4)
      if(length(margin) == k - 1) margin[k] <- margin[k-1]
    .$..margin <- margin
    
    m <- (.$..width - margin[3] - margin[1]) / diff(xlim)
    px <- round(margin[1] + m * (x - xlim[1]))
    px
  }
  widget$yToPixels <- function(., y) {
    ylim <- .$getYlim()

    margin <- .$..margin

    for(k in 2:4)
      if(length(margin) == k - 1) margin[k] <- margin[k-1]
    .$..margin <- margin
    
    m <- (.$..height - margin[2] - margin[4]) / diff(ylim)
    py <- round(margin[2] + m * (y - ylim[1]))
    ## flip
    .$..height - py
    
  }

  ## convert back into XY form pixes
  ## mouse handlers return h$xy = c(x,y) for coordinates in pixels
  widget$pixelsToXY <- function(., pxy) {
    px <- pxy[1]; py <- pxy[2]
    xlim <- .$xlim; ylim <- .$ylim
    widget <- .$..width; height <- .$..height

    margin <- .$..margin

    mx <- (.$..width - margin[3] - margin[1]) / diff(xlim)
    my <- (.$..height - margin[2] - margin[4]) / diff(ylim)

    x <- (px - margin[1])/mx + xlim[1]
    y <- ((height -py) - margin[2])/my + ylim[1]
    return(c(x,y))
  }
  ## xlim -- if not set.
  widget$getXlim <- function(.) {
    if(!exists("xlim",envir=., inherits=FALSE))
      .$xlim <- c(1,.$..width)
    .$xlim
  }
  widget$getYlim <- function(.) {
    if(!exists("ylim",envir=., inherits=FALSE))
      .$ylim <- c(1,.$..height)
    .$ylim
  }
  ## covert color into rgb for use with col=NA argument
  widget$fixColor <- function(., col) {
    if(col %in% colors())
      col <- paste(col2rgb(col), collapse=",")
    col
  }

  ####
  ## The basic R plot commands as methods for the processing object
  ## several argument are not implemented. -- LAZY --
  ## plot.new -- just sets p$background(). The default background has no method to set
  ##             one can set via p$..background <- "..."
  widget$plot.new <- function(., ...) {
    .$background(.$..background)
  }
  ## set xlim and ylim. missing  log = "", asp=NA, 
  widget$plot.window <- function(., xlim, ylim,...) {
    .$xlim <- xlim
    .$ylim <- ylim
  }
  ## title: ylab does not turn text
  widget$title <- function(., main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                           cex = 1, ...) {
    margin <- .$..margin
    xlim <- .$getXlim(); ylim <- .$getYlim()
    width <- .$..width; height <- .$..height
    if(!is.null(main)) {
      .$textSize(cex * 1.2 * .$..pointsize)
      .$ptext(shQuote(main), width/2, margin[2]/2)
    }
    if(!is.null(sub)) {
      .$textSize(cex * .$..pointsize)
      .$ptext(shQuote(sub), width/2, height)
    }

    if(!is.null(xlab)) {
      .$textSize(cex * .8 * .$..pointsize)
      .$ptext(shQuote(xlab), width/2, height - margin[4]/2)
    }
    if(!is.null(ylab)) {
      .$textSize(cex * .8 * .$..pointsize)
      .$ptext(shQuote(ylab), margin[1]/2, height/2)
    }
  }

  ## axis
  widget$axis <- function(.,side,...) {

    if(missing(side)) side <- 1:2
    xlim <- .$getXlim(); ylim <- .$getYlim()
    px <- pretty(xlim); py <- pretty(ylim)
    
    if(1 %in% side) {
      .$lines(xlim, c(ylim[1], ylim[1]))
      for(i in 1:length(px)) 
        .$text(px[i], ylim[1], px[i], pos=1)
    }
    if(2 %in% side) {
      .$lines(c(xlim[1], xlim[1]), ylim)
      for(i in 1:length(py)) 
        .$text(xlim[1],py[i], py[i], pos=2)
    }
    if(3 %in% side) {
      .$lines(xlim, c(ylim[2], ylim[2]))
    }
    if(4 %in% side) {
      .$lines(c(xlim[2], xlim[2]), ylim)
    }
  }

  ### draw box using lines
  widget$box <- function(., ... ) {
    xlim <- .$getXlim(); ylim <- .$getYlim()
    .$lines(xlim[c(1,2,2,1,1)], ylim[c(1,1,2,2,1)], col=col, ...)
  }

  widget$points <- function(., x, y = NULL, cex=1, col = NA, ...) {
    xy <- xy.coords(x,y)
    xy$x <- .$xToPixels(x)
    xy$y <- .$yToPixels(y)

    ## recycle cex
    cex <- rep(cex, length.out=length(xy$x))
    ## fix and recycle col
    if(is.na(col))
      col <- "black"
    col <- .$fixColor(col)
    col <- rep(col, length.out=length(xy$x))

    for(i in seq_along(xy$x))  {
      .$fill(col[i])
      .$ellipse(xy$x[i], xy$y[i], width=round(cex[i] * 5))
    }
  }
  

  ##
  widget$lines <- function(., x, y = NULL, col=NA, lwd=1, ...) {
    xy <- xy.coords(x,y)
    xy$x <- .$xToPixels(x)
    xy$y <- .$yToPixels(y)

    ## fix and recycle col
    if(is.na(col))
      col <- "black"
    col <- .$fixColor(col)
    col <- rep(col, length(xy$x) - 1)
    ## ditto  lwd
    lwd <- rep(lwd, length(xy$x) -1)

    for(i in 2:length(xy$x)) {
      .$stroke(col[i-1])
      .$strokeWeight(lwd[i-1])
      .$line(xy$x[i-1], xy$y[i-1], xy$x[i], xy$y[i])
    }
    .$stroke(0)
    .$strokeWeight(1)
    
  }

  widget$polygon <- function(., x, y=NULL, col=NA, ...)  {
    ## XXX add in other args later
    xy <- xy.coords(x,y)
    xy$x <- .$xToPixels(x)
    xy$y <- .$yToPixels(y)
    
    if(!is.na(col))
      .$fill(.$fixColor(col))

    .$beginShape(type="'polygon'")
    for(i in 1:length(xy$x))
      .$vertex(xy$x[i], xy$y[i])
    .$endShape()
  }
  widget$rect <- function(., xleft, ybottom, xright, ytop, col=NA, ...) {
    ## recycle col
    col <- rep(col, length=length(xleft))
    ## can be vectori´d
    for(i in 1:length(xleft))
    .$polygon(c(xleft[i], xright[i], xright[i], xleft[i]),
              c(ybottom[i], ybottom[i], ytop[i], ytop[i]),
              col=col[i], ...)
  }

  widget$abline <- function(., a = NULL, b = NULL, h = NULL, v = NULL, coef = NULL, ...) {
    ## draws line depending
    if(!is.null(coef))
      a <- coef[1]; b <- coef[2]
    if(inherits(a,"lm")) {
      b <- coef(a)[2]; a <- coef(a)[1]
    }
    xlim <- .$getXlim()
    if(!is.null(a)) {
      y <- a + b*xlim
      .$lines(xlim, y, ...)
    } else if(!is.null(h)) {
      .$lines(xlim, c(h,h), ...)
    } else if(!is.null(v)) {
      ylim <- .$getYlim()
      .$lines(c(v,v), ylim, ...)
    }
    
  }
  widget$text <- function(., x, y = NULL, labels = seq_along(x), 
          cex = 1, col = NULL, pos = NULL, ...) {

    ## XXX add in other args later
    xy <- xy.coords(x,y)
    xy$x <- .$xToPixels(x)
    xy$y <- .$yToPixels(y)
    
    ## recycle labels if needed
    labels <- rep(labels, length=length(xy$x))
    labels <- as.character(labels)
    labels <- shQuote(labels)
    ## recyle size
    size <- round(.$..pointsize * cex)
    size <- rep(size, length=length(xy$x))

    ## recycle cex
    cex <- rep(cex, length=length(xy$x))
    ## fix and recycle col
    if(is.null(col))
      col <- "black"
    col <- .$fixColor(col)
    col <- rep(col, length(xy$x))

    ## recycle pos
    if(is.null(pos)) pos <- 0
    pos <- rep(pos, length(xy$x))
    
    for(i in 1:length(xy$x)) {
      .$textSize(size[i])
##       if(!is.null(font))
##         .$textFont(font, size[i])
##       else if(is.numeric(cex))
##         .$textSize(size[i])

      .$fill(col[i])

      xoff <- 0; yoff <- 0
      if(pos[i] == 1)
        yoff <-  cex[i]* .$..pointsize
      else if(pos[i] == 2)
        xoff <- -cex[i] * .$..pointsize
      else if(pos[i] == 3)
        yoff <- -cex[i]* .$..pointsize
      else if(pos[i] == 4)
        xoff <- cex[i] * .$..pointsize
      .$ptext(labels[i], xy$x[i] + xoff, xy$y[i] + yoff)
    }
  }


  ##################################################
  ## add after CSS, scripts defined
  container$add(widget,...)

  return(widget)
}

