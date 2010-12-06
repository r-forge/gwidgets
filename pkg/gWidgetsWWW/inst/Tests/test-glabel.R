fn <- "glabel"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- glabel("label text", cont=g)
gseparator(cont=g)
widget1 <- glabel("label with <b>markup</b><br /> new line (needs markup=TRUE)",  cont=g)
gseparator(cont=g)
widget2 <- glabel("label with <b>markup</b><br /> new line", markup=TRUE, cont=g)
gseparator(cont=g)
widget3 <- glabel(c("Multi line", "and markup is TRUE"), markup=TRUE, cont=g)
gseparator(cont=g)
widget3 <- glabel(c("Multi line", "and markup is FALSE"), markup=FALSE, cont=g)
## no handlers

## methods

## ## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "new label text"
})

## ##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
