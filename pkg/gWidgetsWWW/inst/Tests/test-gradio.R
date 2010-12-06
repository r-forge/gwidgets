fn <- "gradio"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gradio(state.name[1:5], cont=g)


## handlers
## changed
addHandlerChanged(widget, handler=function(h,...) {
  galert(svalue(h$obj), parent=w)
})


## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=TRUE), parent=w)
})

gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=FALSE), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- state.name[2]
})

gbutton("click to set svalue<- by index ", cont=g, handler=function(h,..) {
  svalue(widget, index=TRUE) <- 3
})



## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget[]), collapse="<br />"), parent=w)
})


##[<- no method

##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
