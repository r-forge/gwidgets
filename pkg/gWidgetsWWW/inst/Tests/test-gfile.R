fn <- "gfile"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gfile(text="select a file", cont=g)


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


##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
