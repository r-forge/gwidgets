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


##            asCairoDevice(da, pointsize = ps)

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
##             addhandler(obj,signal="unmap", handler = function(...) {
##               print("unmap")
##               return(TRUE)
##             })

            ## in the new cairoDevice this gives problems
            ## close this device when unrealized
#            addhandlerunrealize(obj,
#                                handler = function(h,...) {
#                                  ## if not windows, we shut down device
#                                  if(.Platform$OS != "windows")
#                                    try(dev.off(tag(obj,"device")), silent=TRUE)
#                                  return(TRUE)
  
            ## raise this device when clicked
##            ID = addhandlerclicked(obj,
##               handler=function(h,...) {
##                 ### visible(h$obj) <- TRUE
##                 theDevice = tag(obj,"device")
##                 dev.set(theDevice)      # working?
##                 return(TRUE)
##               })

            ## does sleeping for a bit help out with the 
            ## Error in plot.new() : figure margins too large
            Sys.sleep(.25)
            
            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }
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
            }
            
            id = addhandler(obj,signal = "button-press-event",handler=f, action=action)
            invisible(id)
          })

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
