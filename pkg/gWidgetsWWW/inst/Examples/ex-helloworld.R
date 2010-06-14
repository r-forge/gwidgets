w <- gwindow("Hello world example")
b <- gbutton("click me", cont = w)
addHandlerClicked(b, handler = function(h,...) {
  gmessage("Hello world", parent=w)
})

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
