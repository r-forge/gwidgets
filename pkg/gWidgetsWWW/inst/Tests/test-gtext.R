fn <- "gtext"

w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

widget <- gtext("edit me", cont=g)


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

gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "New text"
})

gbutton("click to set svalue<- multiline ", cont=g, handler=function(h,..) {
  svalue(widget) <- c("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor",
                      "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
                      "exercitation ullamco",
                      "laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure",
                      "dolor in reprehenderit in voluptate velit")
})


gbutton("click to add text",  cont=g, handler=function(h,..) {
  insert(widget, "This text is inserted")
})

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
