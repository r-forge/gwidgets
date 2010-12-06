fn <- "gdf"

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gdf(data.frame(a=1:5,b=letters[1:5], c=c(T,T,F,F,F), stringsAsFactors=FALSE), cont=g)
size(widget) <- c(400,150)
## handlers
## changed
addHandlerChanged(widget, handler=function(h,..) {
  galert(svalue(h$obj), parent=w)
})

## click, doubleclick, ...

## methods

## ## svalue
## gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
##   svalue(widget) <- 1
## })

## gbutton("click to get svalue", cont=g, handler=function(h,..) {
##   galert(paste(svalue(widget), collapse=","), parent=w)
## })

## gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
##   galert(paste(svalue(widget, index=TRUE), collapse=", "), parent=w)
## })

## gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
##   galert(paste(svalue(widget, index=FALSE), collapse=", "), parent=w)
## })


## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget[]), collapse="<br />"), parent=w)
})

##[<-
gbutton("click to set [<- ", cont=g, handler=function(h,..) {
  widget[1,1] <- 2
})

                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
