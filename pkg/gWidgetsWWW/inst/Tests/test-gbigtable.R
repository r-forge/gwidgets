fn <- "gbigtable"
require(MASS, quietly=TRUE)

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gbigtable(Aids2, cont=g)
size(widget) <- c(600,400)
## handlers
## changed
addHandlerChanged(widget, handler=function(h,..) {
  galert(svalue(h$obj), parent=w)
})

## click, doubleclick, ...

## methods

## svalue
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- 1
})

gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=TRUE), parent=w)
})

gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=FALSE), parent=w)
})


## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget[1:5,]), collapse="<br />"), parent=w)
})

##[<-
gbutton("click to set [<- ", cont=g, handler=function(h,..) {
  widget[] <- Aids2[1:100,]             #shorten
})

                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
