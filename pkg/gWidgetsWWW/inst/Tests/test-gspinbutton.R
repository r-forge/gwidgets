fn <- "gspinbutton"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gspinbutton(from=0, to=100, by=5, cont=g)


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

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- 50
})


##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
