fn <- "gcheckbox"

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gcheckboxgroup(state.name[1:5], selected=FALSE, cont=g)
## handlers

## changed
addHandlerChanged(widget, handler=function(h,..) {
  galert(svalue(h$obj), parent=w)
})

## click, doubleclick, ...

## methods

## svalue
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- c(T,F,T,F,T)
})

gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget), collapse=","), parent=w)
})

gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget, index=TRUE), collapse=", "), parent=w)
})

gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget, index=FALSE), collapse=", "), parent=w)
})


## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget[]), collapse="<br />"), parent=w)
})

## ##[<- none


gseparator(cont=g)
widget1 <- gcheckboxgroup(state.name[1:5], use.table=TRUE, selected=TRUE, cont=g)
size(widget1) <- c(400,150)

## changed
addHandlerChanged(widget1, handler=function(h,..) {
  galert(svalue(h$obj), parent=w)
})

## svalue
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget1) <- c(T,F,T,F,T)
})

gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget1), collapse=","), parent=w)
})

gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget1, index=TRUE), collapse=", "), parent=w)
})

gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
  galert(paste(svalue(widget1, index=FALSE), collapse=", "), parent=w)
})


## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget1[]), collapse="<br />"), parent=w)
})

                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
