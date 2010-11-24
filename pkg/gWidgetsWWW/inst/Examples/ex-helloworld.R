## Simplest example with a callback

w <- gwindow("Hello world example")
b <- gbutton("click me", cont = w)
addHandlerClicked(b, handler = function(h,...) {
  gmessage("Hello world", parent=w)
})
## show off
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
