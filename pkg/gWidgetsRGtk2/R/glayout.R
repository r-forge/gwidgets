setClass("gLayoutRGtk",
         contains="gContainerRGtk",
         prototype=prototype(new("gContainerRGtk"))
         )

## an gWidget for tables
 

setMethod(".glayout",
          signature(toolkit="guiWidgetsToolkitRGtk2"),
          function(toolkit,
                   homogeneous = FALSE,
                   spacing = 10,        # amount (pixels) between row, cols, NULL=0
                   container = NULL, ...
                   ) {
            
            force(toolkit)

            tbl <- gtkTableNew(homogeneous = homogeneous)
            ## homogeneous spacing
            tbl$SetRowSpacings(spacing)
            tbl$SetColSpacings(spacing)
            
            obj <- as.gWidgetsRGtk2(tbl)

            if (!is.null(container)) {
              if(is.logical(container) && container == TRUE)
                container = gwindow()
              add(container, obj,...)
            }
            
            invisible(obj)
          })
            

as.gWidgetsRGtk2.GtkTable <- function(widget, ...) {
  obj = new("gLayoutRGtk", block=widget, widget=widget,
    toolkit=guiToolkit("RGtk2"))

  return(obj)
}


            
### The add method is a stub so that this works with same
## approach as gWidgetstcltk
setMethod(".add",
          signature(toolkit="guiWidgetsToolkitRGtk2", obj="gLayoutRGtk", value="gWidgetRGtk"),
          function(obj, toolkit, value, ...) {
            ## stub
          })



## how we populate the table
setReplaceMethod("[",
                 signature(x="gLayoutRGtk"),
                 function(x, i, j,..., value) {
                   .leftBracket(x, x@toolkit, i, j, ...) <- value
                   return(x)
                 })

setReplaceMethod(".leftBracket",
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gLayoutRGtk"),
          function(x, toolkit, i, j, ..., value) {

            obj <- x
            tbl <- getWidget(obj)
            
            if(missing(i) || missing(j)) {
              cat(gettext("glayout: [ needs to have both i and j specified."))
              return(x)
            }

            
            ## check that all is good
            if(is.character(value)) {
              ## wrap characters into labels
              value <- glabel(value,...)
            }
            args <- list(...)
            expand <- args$expand; if(is.null(expand)) expand <- FALSE
            
            ## fix up widget alignment if asked via anchor
            if(!is.null(args$anchor)) {
              anchor <- args$anchor
              anchor <- (anchor + 1)/2; anchor[2] <- 1 - anchor[2]

              child <- getBlock(value)
              childWidget <- getWidget(value)
              ## but not so fast, not all components have xalign, yalign
              ## property

              ## in gtkstuff 
              setXYalign(child, childWidget, anchor)
              
              ## XXX this is slower
##               if('xalign' %in% names(child) && class(child)[1] != "GtkEntry") 
##                 child['xalign'] <- anchor[1]
##               else if('xalign' %in% names(childWidget)
##                       && class(childWidget) != "GtkEntry") 
##                 childWidget['xalign'] <- anchor[1]

##               if('yalign' %in% names(child)) 
##                 child['yalign'] <- anchor[2]
##               else if('yalign' %in% names(childWidget)) 
##                 childWidget['yalign'] <- anchor[2]
            }

            ## fix up number of columns
            d <- dim(obj)
            nr <- max(j); nc <- max(i)
            if( nr > d[1] || nc > d[2])
              tbl$Resize(max(max(j), nr), max(max(i), nc))
              
            if(expand)
              opts <- c("fill","expand","shrink")
            else
              opts <- c("fill")
            
            child <- getBlock(value)
            tbl$Attach(child,
                       min(j)-1, max(j), min(i)-1, max(i),
                       xoptions=opts,yoptions=opts)

            return(x)
          })

## inherits delete method for containers

## replaced
## We like visible, return it. Unlike delete it only hides the widget
## setReplaceMethod(".visible",
##                  signature(toolkit="guiWidgetsToolkitRGtk2",obj="gLayoutRGtk"),
##                  function(obj, toolkit, ..., value) {
##                    gwCat(gettext("visible<- is now redundant for glayout in RGtk2"))
##                    return(obj)
##                  })

## get number of rows and columns
setMethod(".dim", 
          signature(toolkit="guiWidgetsToolkitRGtk2",x="gLayoutRGtk"),
          function(x,toolkit) {
            tbl <- getWidget(x)
            return(c(nrow=tbl$GetNrows(), ncol=tbl$GetNcols()))
          })
