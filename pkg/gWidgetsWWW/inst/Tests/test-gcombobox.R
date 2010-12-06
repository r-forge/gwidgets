fn <- "gcombobox"

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gcombobox(state.name[1:5],  cont=g)
## handlers

 ## changed
 addHandlerChanged(widget, handler=function(h,..) {
   galert(svalue(h$obj), parent=w)
 })

 ## click, doubleclick, ...

 ## methods

 ## svalue
 gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
   svalue(widget) <- "California"
 })

 gbutton("click to set svalue<- index=TRUE, 0 ", cont=g, handler=function(h,..) {
   svalue(widget, index=TRUE) <- 0
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
 d <- data.frame(states=state.name[1:5], icon=getStockIcons()[1:5], qtip=state.name[1:5], stringsAsFactors=FALSE)
 widget1 <- gcombobox(d,  cont=g)

 ## changed
 addHandlerChanged(widget1, handler=function(h,..) {
   galert(svalue(h$obj), parent=w)
 })

 ## svalue
 gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
   svalue(widget1) <- "California"
 })

 gbutton("click to set svalue<-, index=TRUE ", cont=g, handler=function(h,..) {
   svalue(widget1, index=TRUE) <- 3
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


gseparator(cont=g)
glabel("Type ahead look", cont=g)
widget2 <- gcombobox(d[,1], selected=0, editable = TRUE, cont = g)
widget2$..hideTrigger <- TRUE
                                        

                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
