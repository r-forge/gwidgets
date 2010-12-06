fn <- "gstatusbar"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gstatusbar("status text", cont=g)



## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "New status text"
})


##################################################                  
#gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
