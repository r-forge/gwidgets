## cairo graphics device
## would like to get size from par("fin"), but this isn't so easy as it
## seems to pop up a new plot container

### Trouble when adding to a notebook. Currently when a notebook page is closed the signal to close the widget is not propogated.


setClass("gGraphicsRGtk",
         contains="gComponentRGtk",
         prototype=prototype(new("gComponentRGtk"))
         )

setMethod(".ggraphics",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,
                   dpi=75, ps=12,
                   container=NULL,...) {

            force(toolkit)
            
            require(cairoDevice)
            
            da <- gtkDrawingAreaNew()
            ## allow events on this widget
            da$AddEvents(GdkEventMask["all-events-mask"])
            if(!is.null(width) & !is.null(height))
              da$setSizeRequest(width, height)


            obj <- as.gWidgetsRGtk2(da)
#            obj = new("gGraphicsRGtk",block=da, widget=da, toolkit=toolkit)


            ## Woah Nelly! since 2.0.1 the device needs to be realized before we can make it
            ## so we put this in: 
            ## when a device is clicked.
            
            addhandler(obj,signal="map",handler = function(h, ...) {
              da <- h$action
              ## in cairoDevice (>= 2.2.0) the device is stored in da$GetData(".devnum")
              if(is.null(da$GetData(".devnum"))) {
                 asCairoDevice(da, pointsize=ps) # turn into cairo device
                 tag(obj,"device") <- da$GetData(".devnum")
               }
##                if(is.null(tag(obj,"device"))) {
##                  asCairoDevice(da, pointsize=ps) # turn into cairo device
##                  tag(obj,"device") <- dev.cur()  # now we can set device, as it is realized and now drawable
##                }
               return(TRUE)             # don't propogate
             }, action=da)

            ## handlers to raise device when clicked upon. This seems a natural way to interact with
            ## the device
            .getDevNo <- function(da) da$getData(".devnum")
            .setDevNo <- function(da, ...) {
              dev.set(.getDevNo(da))
              ## indicate?
              
              FALSE}
            ## raise when click into window
            gSignalConnect(da, "button-press-event", f=.setDevNo)
            ## raise when motion over device
            da$addEvents(GdkEventMask['enter-notify-mask'])
            gSignalConnect(da, "enter-notify-event", f=.setDevNo)
            ## close device when destroyed
            gSignalConnect(da, "destroy-event", f=function(da, ...) {
              dev.off(.getDevNo(da))
              return(FALSE)
            })


            ## Add rubber banding
            ## This code is borrowed from the excellent playwith package by Felix Andrews

            ## add environment and values to da
            e <- environment()
            e$dragging <- FALSE
            e$x0 <- e$y0 <- e$x <- e$y <- 0
            da$setData("env", e)

            ## need to bind drag actions: click, motion, release

            gSignalConnect(da, "button-press-event", function(w, e) {
              da <- w
              daClearRectangle(da)
              
              da.w <- da$getAllocation()$width
              da.h <- da$getAllocation()$height
              buf <- gdkPixbufGetFromDrawable(src=da$window, src.x=0, src.y=0,
                                              dest.x=0, dest.y=0, width=da.w, height=da.h)
              w$setData("buf", buf)
              env <- w$getData("env")
              env$x0 <- env$x <- e$x
              env$y0 <- env$y <- e$y
              env$dragging <- TRUE
              return(FALSE)
            })
            
            gSignalConnect(da, "motion-notify-event", function(w, e) {
              env <- w$getData("env")
              ## are we dragging?
              if(env$dragging) {
                daClearRectangle(w)
                
                env$x <- e$x
                env$y <- e$y
                
                ## did we move enough? 10 pixels say
                
                if(max(abs(env$x - env$x0), abs(env$y - env$y0)) > 10)
                  daDrawRectangle(w, env$x0, env$x, env$y0, env$y)
                
                
              }
              return(FALSE)
            })
            
            gSignalConnect(da, "button-release-event", function(w, e) {
              env <- w$getData("env")
              ## remove draggin
              env$dragging <- FALSE
              return(FALSE)
            })
            


            ## Add to container if requested
            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }

            ## after realization, process events
            ## fixes (sometimes) the issue with plot.new having too small margins --
            ## got this from playwith
            gdkWindowProcessAllUpdates()
            while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
            
            return(obj)
          })

as.gWidgetsRGtk2.GtkDrawingArea <- function(widget,...) {
  obj <- new("gGraphicsRGtk",block=widget, widget=widget,
             toolkit=guiToolkit("RGtk2"))
  return(obj)
}


as.gGd = function(obj) {
  if(inherits(obj,"GtkDrawingArea")) {
    newobj = list(ref = obj, device = obj$GetData("device"))
    class(newobj) <- c("gGd", "gComponent")
    return(newobj)
  } else {
    cat(gettext("conversion failed\n"))
    return(obj)
  }
}


### methods

## adding to a group is funny, we intercept here
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGroupRGtk", value="gGraphicsRGtk"),
          function(obj, toolkit, value, ...) {
            getWidget(obj)$PackStart(value@block, TRUE, TRUE, 0) # expand to fill if TRUE
          })



## raise this device
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGraphicsRGtk"),
                 function(obj, toolkit, ..., value) {
                   if(is.logical(value) == TRUE) {
                     dev.set(tag(obj,"device"))
                   }
                   return(obj)
                 })

## save Current Page
## This uses GTK -- not R to save.
## need to have window fully shown
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGraphicsRGtk"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   if(length(value) > 1) {
                     file = value$file
                     extension  = value$extension
                   } else {
                     file = value; extension = ""
                   }
                   ## check that filename is okay
                   if(!is.null(file) && !is.null(extension)) {
                     tmp = unlist(strsplit(file,"\\."))
                     if(tmp[length(tmp)] != extension) {
                       filename = Paste(file,".",extension)
                     } else {
                       filename = file
                     }
                   } else {
                     return()
                   }
                   
                   drawarea = obj@widget
                   
                   parentAllocation = drawarea$GetParent()$GetAllocation()
                   
                   pixbuf = gdkPixbufGetFromDrawable(
                     src=drawarea$GetWindow(),
                     cmap=drawarea$GetColormap(),
                     src.x = drawarea$GetAllocation()$x - parentAllocation$x,
                     src.y = drawarea$GetAllocation()$y - parentAllocation$y,
                     dest.x = 0,
                     dest.y = 0,
                     width = drawarea$GetAllocation()$width,
                     height = drawarea$GetAllocation()$height
                     )
                   pixbuf$Save(filename = filename,type=extension)
                   
                   ##   switch(extension,
                   ##          "ps" = dev.copy2eps.hack(file=filename,
                   ##            onefile=onefile, horizontal=horizontal,
                   ##            width=width, height = height),
                   ##          "eps" = dev.print.hack(postscript,file=filename,
                   ##            onefile=onefile, horizontal=horizontal,
                   ##            width=width, height = height),
                   ##          "pdf" = dev.print.hack(pdf,file=filename,
                   ##            onefile=onefile, horizontal=horizontal,
                   ##            width=width, height = height),
                   ##          "jpg" = dev.print.hack(jpeg,file=filename,
                   ##            onefile=onefile, horizontal=horizontal,
                   ##            width=width, height = height),
                   ##          "jpeg" = dev.print.hack(jpeg,file=filename,width=width,height=height),
                   ##          "png" = dev.print.hack(png,file=filename,width=width,height=height),
                   ##          cat("***\n Don't know this extension:", type,"\n\n")
                   ##          )
                   
                   return(obj)
                 })


### handlers
## add this expose event for graph
setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGraphicsRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"expose-event",handler,action,...)
          })

## applies a handler to the mouse click. The handler gets extra
## argument h$x, h$y passed into it. These are in [0,1] coordinates
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGraphicsRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ## handler has $obj for obj clicked on, $x, $y, $action

            ## convert from "plt" coordinates to more familiar "usr"
            pltToUsr = function(x,y) {
              plt = par("plt"); usr = par("usr")
              c( (usr[2]-usr[1])/(plt[2]-plt[1])*(x - plt[1]) + usr[1],
                (usr[4] - usr[3])/(plt[4] - plt[3])*(y - plt[3]) + usr[3])
            }


            f = function(h,w,e,...) {
              allocation = w$GetAllocation()
              xclick = e$GetX()
              yclick = e$GetY()
              x = xclick/allocation$width
              y = (allocation$height - yclick)/allocation$height

              ## put into usr coordinates
              tmp = pltToUsr(x,y)
              h$x = tmp[1]
              h$y = tmp[2]

              
              handler(h,...)
              return(FALSE)
            }
            
            id = addhandler(obj,signal = "button-press-event",handler=f, action=action)
            invisible(id)
          })


##' Changed handler is called after rubber band selection is updated
##'
##' Just click and drag out a rubber band
##' The "h" list has components
##' h$x for the x values in NDC coordinates
##' h$y for the y values in NDC coordinates
##' These can be converted as in grconvertX(h$x, from="ndc", to="user")
setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkitRGtk2",obj="gGraphicsRGtk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            da <- getWidget(obj)
            ID <- gSignalConnect(da, "button-release-event", function(w, e) {
              coords <- drawableToNDC(w)
              h <- list(obj=obj,
                        action=action,
                        x=grconvertX(coords$x, from="ndc", to="user"),
                        y=grconvertY(coords$y, from="ndc", to="user"))
              handler(h, ...)
              return(FALSE)             # propagate
            })
          })

##' Draw a rectangle for rubber banding
daDrawRectangle <- function(da,  x0, x, y0, y) {

  x <- c(x0,x); y <- c(y0, y)
  x0 <- min(x); x <- max(x)
  y0 <- min(y); y <- max(y)
  

  da.w <- da$getAllocation()$width
  da.h <- da$getAllocation()$height

  ## background style
  gcb <- gdkGCNew(da$window)
  gcb$copy(da["style"]$blackGc)
  gcb$setRgbFgColor(gdkColorParse("gray50")$color)
  gcb$setLineAttributes(line.width=1, line.style=GdkLineStyle["solid"],
                        cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
  ## foreground style
  gc <- gdkGCNew(da$window)
  gc$copy(da["style"]$blackGc)
  gc$setRgbFgColor(gdkColorParse("black")$color)
  gc$setRgbBgColor(gdkColorParse("gray50")$color)
  gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
                       cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
  gc$setDashes(c(8, 4))

  ## the entire rectangle to clear
  rect <- as.GdkRectangle(c(x=0, y=0, width=da.w, height=da.h))
  da$setData("lastRect", rect)

  for (i in 1:2) {
    ## draw in background color first
    tmp.gc <- if (i == 1) gcb else gc
    gdkDrawRectangle(da$window, gc=tmp.gc, filled=FALSE, x=x0, y=y0, width=x-x0, height=y-y0)
  }

  gdkWindowProcessAllUpdates()
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
   

}

##' clear all rectangles that came from rubber banding
daClearRectangle <- function(da) {

  last <- da$getData("lastRect")
  if(!is.null(last))
    da$window$invalidateRect(last, FALSE)

  gdkWindowProcessAllUpdates()
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}

##' convert rectangle on drawable into NDC coordinates
drawableToNDC <- function(da) {
  ## convert to normalized device coordinates
  e <- da$getData("env")
  x.pixel <- sort(c(e$x0, e$x))
  y.pixel <- sort(c(e$y0, e$y))
  
  da.w <- da$getAllocation()$width
  da.h <- da$getAllocation()$height

  ndc <- list(x=x.pixel/da.w, y= 1- rev(y.pixel/da.h))
  return(ndc)
}
##################################################
##
## dev.print and dev.copy2eps have a test on the device that needs Cairo added to it
devPrintHack = function (device = postscript, ...) 
{
  current.device <- dev.cur()
  nm <- names(current.device)[1]
  if (nm == "null device") 
    stop("no device to print from")
  if (!(nm %in% c("Cairo", "X11", "GTK", "gnome", "windows", "quartz"))) 
    stop("can only print from screen device")
  oc <- match.call()
  print(oc)
  oc[[1]] <- as.name("dev.copy")
  oc$device <- device
  din <- par("din")
  w <- din[1]
  h <- din[2]
  if (missing(device)) {
    if (is.null(oc$file)) 
      oc$file <- ""
    hz0 <- oc$horizontal
    hz <- if (is.null(hz0)) 
      ps.options()$horizontal
    else eval.parent(hz0)
    paper <- oc$paper
    if (is.null(paper)) 
      paper <- ps.options()$paper
    if (paper == "default") 
      paper <- getOption("papersize")
    paper <- tolower(paper)
    switch(paper, a4 = {
      wp <- 8.27
      hp <- 11.69
    }, legal = {
      wp <- 8.5
      hp <- 14
    }, executive = {
      wp <- 7.25
      hp <- 10.5
    }, {
      wp <- 8.5
      hp <- 11
    })
    wp <- wp - 0.5
    hp <- hp - 0.5
    if (!hz && is.null(hz0) && h < wp && wp < w && w < hp) {
      hz <- TRUE
    }
    else if (hz && is.null(hz0) && w < wp && wp < h && h < 
             hp) {
      hz <- FALSE
    }
    else {
      h0 <- ifelse(hz, wp, hp)
      if (h > h0) {
        w <- w * h0/h
        h <- h0
      }
      w0 <- ifelse(hz, hp, wp)
      if (w > w0) {
        h <- h * w0/w
        w <- w0
      }
    }
    if (is.null(oc$pointsize)) {
      pt <- ps.options()$pointsize
      oc$pointsize <- pt * w/din[1]
    }
    if (is.null(hz0)) 
      oc$horizontal <- hz
    if (is.null(oc$width)) 
      oc$width <- w
    if (is.null(oc$height)) 
      oc$height <- h
  }
  else {
    devname <- deparse(substitute(device))
    if (devname %in% c("png", "jpeg", "bmp") && is.null(oc$width) && 
        is.null(oc$height)) 
      warning("need to specify one of 'width' and 'height'")
    if (is.null(oc$width)) 
      oc$width <- if (!is.null(oc$height)) 
        w/h * eval.parent(oc$height)
      else w
    if (is.null(oc$height)) 
      oc$height <- if (!is.null(oc$width)) 
        h/w * eval.parent(oc$width)
      else h
  }
  dev.off(eval.parent(oc))
  dev.set(current.device)
}

dev.copy2eps.hack = function (...) 
{
  current.device <- dev.cur()
  nm <- names(current.device)[1]
  if (nm == "null device") 
    stop("no device to print from")
  if (!(nm %in% c("Cairo","X11", "GTK", "gnome", "windows", "quartz"))) 
    stop("can only print from screen device")
  oc <- match.call()
  
  
  oc[[1]] <- as.name("dev.copy")
  oc$device <- postscript
  oc$onefile <- FALSE
  oc$horizontal <- FALSE
  if (is.null(oc$paper)) 
    oc$paper <- "special"
  din <- par("din")
  w <- din[1]
  h <- din[2]
  if (is.null(oc$width)) 
    oc$width <- if (!is.null(oc$height)) 
      w/h * eval.parent(oc$height)
    else w
  if (is.null(oc$height)) 
    oc$height <- if (!is.null(oc$width)) 
      h/w * eval.parent(oc$width)
    else h
  if (is.null(oc$file)) 
    oc$file <- "Rplot.eps"
  dev.off(eval.parent(oc))
  dev.set(current.device)
} 
