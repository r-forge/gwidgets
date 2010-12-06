fn <- "gsvg"

require(RSVGTipsDevice, quietly=TRUE, warn=FALSE) # must require within callback
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

f <- getStaticTmpFile(ext=".svg")
devSVGTips(f, toolTipMode=2, toolTipOpacity=.8)
plot(mpg ~ wt, mtcars)
dev.off()

widget <- gsvg(f, cont=g)

## handlers
## none
## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  f <- getStaticTmpFile(ext=".svg")
  devSVGTips(f, toolTipMode=2, toolTipOpacity=.8)
  plot(1/mpg ~ wt, mtcars)
  dev.off()
  svalue(widget) <- f
})

##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
