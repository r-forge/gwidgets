## Basic test to see if gWidgetsWWW works for a simple GUI

require(gWidgetsWWW, quietly=TRUE)
w <- gwindow("Test of a window")
g <- ggroup(horizontal=FALSE, cont=w)
b <- gbutton("button", cont=g, handler = function(h,...) {
  gmessage(svalue(b), parent=b)
})
svalue(b)
svalue(b) <- "new label text"
## show it
w
