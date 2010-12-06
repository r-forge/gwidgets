fn <- "gimage"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gimage("http://www.r-project.org/Rlogo.jpg", cont=g)

## handlers
## none
## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  si <- getStockIcons()
  svalue(widget) <- si[1]
})

##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
