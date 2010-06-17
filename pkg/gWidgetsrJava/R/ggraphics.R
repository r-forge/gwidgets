## cairo graphics device
## would like to get size from par("fin"), but this isn't so easy as it
## seems to pop up a new plot container

### Trouble when adding to a notebook. Currently when a notebook page is closed the signal to close the widget is not propogated.


setClass("gGraphicsrJava",
         contains="gComponentrJava",
         prototype=prototype(new("gComponentrJava"))
         )

setMethod(".ggraphics",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,
                   dpi=75, ps=12,
                   container=NULL,...) {

            force(toolkit)

            ## this is from iWidgets code in widgets.R
            ## This doesn't allow us to embed the device into a widget

            dev.new()
##            JavaGD("ggraphics", width = width, height = height, ps = ps)

            obj <- glabel("No embeddable device\nin gWidgetsrJava", cont = container)
            return(obj)

            ## This used to work, now doesn't. Just punt as above by
            ## calling a new device through JavaGD.
            ## Methods won't work

            gd = .jnew("org/rosuda/javaGD/GDCanvas", as.integer(force(width)),as.integer(force(height)))

##             di <- dev.cur()

            ## Do I need to mess with devices?
            
            obj = new("gGraphicsrJava",block=gd, widget=gd,
              toolkit=toolkit,ID=getNewID(),  e = new.env())
            tag(obj,"device") <- dev.cur()
  
            ## raise this device when clicked
            ID = addhandlerclicked(obj,
              handler=function(h,...) {
                visible(h$obj) <- TRUE
              })

            ## attach?
            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj, ...)
            }
            return(obj)
          })


### methods



## raise this device
setReplaceMethod(".visible",
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gGraphicsrJava"),
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
                 signature(toolkit="guiWidgetsToolkitrJava",obj="gGraphicsrJava"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   cat("svalue not implemented\n")
                   return(obj)
                 })


### handlers
## add this expose event for graph
setMethod(".addhandlerexpose",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGraphicsrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            addhandler(obj,"expose-event",handler,action)
          })

## applies a handler to the mouse click. The handler gets extra
## argument h$x, h$y passed into it. These are in [0,1] coordinates
setMethod(".addhandlerclicked",
          signature(toolkit="guiWidgetsToolkitrJava",obj="gGraphicsrJava"),
          function(obj, toolkit, handler, action=NULL, ...) {
            ## handler has $obj for obj clicked on, $x, $y, $action
            
            f = function(h,w,e,...) {
              allocation = w$GetAllocation()
              xclick = e$GetX()
              yclick = e$GetY()
              h$x = xclick/allocation$width
              h$y = (allocation$height - yclick)/allocation$height
              
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
