## I should make an abstract class for gButton, gImage and gLabel
## instead I get lots of repeated code.


setClass("gImagetcltk",
         contains="gComponenttcltk",
         prototype=prototype(new("gComponenttcltk"))
         )

## image use 


setMethod(".gimage",
          signature(toolkit="guiWidgetsToolkittcltk"),
          function(toolkit,
                   filename = "", dirname="",
                   size="",
                   handler=NULL, action=NULL, 
                   container=NULL, ...) {

            force(toolkit)

            ## container in tcltk
            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning(gettext("Container argument is not correct: No NULL containers possible in gWidgetstcltk\n"))
              return()
            }


            
##             ## get filename
##             iconFile = NULL
##             if(dirname == "stock") {
##               ## check if in gWidgetstcltk
##               isIcon <- system.file(paste("icons/",filename,".gif",sep=""),
##                                     package="tcltk")
##               if(file.exists(isIcon)) {
##                 iconFile <- isIcon
##               } else {
##                 gWidgetstcltkIcons = getStockIcons()
##                 iconFile = gWidgetstcltkIcons[[filename,exact=TRUE]]
##                 if(!is.null(iconFile) && !file.exists(iconFile)) {
##                   iconFile <- gWidgetstcltkIcons[["clear"]]
##                 }
##               }
##             } else if(dirname != "") {
##               iconFile = paste(dirname,filename,sep=.Platform$file.sep)
##             } else {
##               iconFile = filename
##             }

##             imageID = paste("gimage",gp$ID,sep="")

##             ## base tk support gif, ppm and bitmap (ppm doesn't seem to though)
##             if(!is.null(iconFile) && file.exists(iconFile)) {
##               x = try(tcl("image","create","photo",imageID,file=iconFile),silent=TRUE)
##               ## now try as bitmap
##               if(inherits(x,"try-error")) {
##                 x = try(tcl("image","create","bitmap",imageID,file=iconFile),silent=TRUE)
##               }
##               if(inherits(x,"try-error")) {
##                 cat(gettext("gimage had issues. Only gif, ppm and xbm files  in gWidgetstcltk\n"))
##                 lab <- ttklabel(gp,text="")
##               } else {
##                 lab <- ttklabel(gp, image=imageID)
##               }
##             } else {
##               ##  uninitialized
##               lab <- ttklabel(gp,text="")
##             }
##             tkpack(lab, expand=TRUE, fill="both")


            ## we need the imageID (tcl name for image)
            ## for stockicons we have that ::stockicon::quit.fig
            ## returned by findIcon()
            ## for non stock, we need to make. For this we need iconFile -- path
            ## and make a image id
            iconFile <- NULL
            imageID <- ""
            if(dirname == "stock") {
              imageID <- findIcon(filename)
            } else {
              if(dirname != "") {
                iconFile = paste(dirname,filename,sep=.Platform$file.sep)
              } else {
                iconFile = filename
              }
              imageID = paste("::gimage::",filename,sep="")
              if(!is.null(iconFile) && file.exists(iconFile)) {
                x = try(tcl("image","create","photo",imageID,file=iconFile),silent=TRUE)
                ## now try as bitmap
                if(inherits(x,"try-error")) {
                  x = try(tcl("image","create","bitmap",imageID,file=iconFile),silent=TRUE)
                }
                if(inherits(x,"try-error")) {
                  cat(gettext("gimage had issues. Only gif, ppm and xbm files  in gWidgetstcltk\n"))
                  imageID <- ""
                }
              }
            }
            
            ## implement size -- photo has width, height
            if (size != "") cat(gettext("gimage: size argument is currently ignored\n"))


            tt <- getWidget(container)
            lab <- ttklabel(tt, text="")

            if(imageID != "")
              tkconfigure(lab, image=imageID)
            
            obj = new("gImagetcltk", block=lab, widget=lab,
              toolkit=toolkit,ID=getNewID(),e = new.env()
              )

            tag(obj,"filename") <- iconFile
            tag(obj,"imageID") <- imageID
            
            if(!is.null(handler)) {
              id = addhandlerclicked(obj, handler=handler, action=action)
            }

            ## attach
            add(container, obj,...)
            
            invisible(obj)
          })
          
          
setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            ## return name?
            return(tag(obj,"filename"))
          })

setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
                 function(obj, toolkit, index=NULL,  ..., value) {
                   ## value is a full filename or icon name
                   gWidgetstcltkIcons = getStockIcons()

                   
                   if(!file.exists(value)) {
                     imageID <- findIcon(value)
                     tkconfigure(getWidget(obj),image=imageID)
                   } else {
                     x = try(tcl("image","create","photo",tag(obj,"imageID"),file=value), silent=TRUE)
                     if(inherits(x,"try-error")) {
                       cat(gettext("Only gif and pnm files are possible in gWidgetstcltk\n"))
                     } else {
                       tkconfigure(getWidget(obj),image=tag(obj,"imageID"))
                     }
                     ## store dynamically, not with @filename
                     tag(obj,"filename") <- value
                   }
                   
                   return(obj)
                 })


## set size
setReplaceMethod(".size", 
                 signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
                 function(obj, toolkit, ..., value) {
                   ## pixels for tkframe etc
                   tkconfigure(getWidget(obj), width=value[1], height=value[2])
                   return(obj)
                 })


setMethod(".addhandlerchanged",
          signature(toolkit="guiWidgetsToolkittcltk",obj="gImagetcltk"),
          function(obj, toolkit, handler, action=NULL, ...) {
            .addhandlerclicked(obj, toolkit, handler, action, ...)
          })