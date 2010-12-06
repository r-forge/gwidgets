fn <- "ghtml"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- ghtml("Some <b>html</b>text", cont=g)

## handlers
## none
## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "New <b>text</b> with x&apos;s in there"
})

gbutton("click to set svalue<-, encode=TRUE ", cont=g, handler=function(h,..) {
  svalue(widget, encode=TRUE) <- "New <b>text</b> with x's in there"
})

widget1 <- ghtml(asURL("http://www.r-project.org/Rlogo.jpg"), cont=g)

##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
