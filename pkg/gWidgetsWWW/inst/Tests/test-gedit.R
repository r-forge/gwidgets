fn <- "gedit"

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gedit("edit me", cont=g)


## handlers
## changed
addHandlerChanged(widget, handler=function(h,...) {
  galert(svalue(h$obj), parent=w)
})

addHandlerKeystroke(widget, handler=function(h,...) {
  key <- h$key
  galert(key, parent=w)
})

## methods

## svalue
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "New text"
})

gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## ## gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
## ##   galert(svalue(widget, index=TRUE), parent=w)
## ## })

## ## gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
## ##   galert(svalue(widget, index=FALSE), parent=w)
## ## })


## ## [
## gbutton("click to get [", cont=g, handler=function(h,..) {
##   galert(paste(capture.output(widget[]), collapse="<br />"), parent=w)
## })

## ##[<-
## gbutton("click to set [<- ", cont=g, handler=function(h,..) {
##   widget[] <- "new label"
## })

                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
