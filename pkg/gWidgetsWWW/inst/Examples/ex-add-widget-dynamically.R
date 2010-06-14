## can we add to a group?
w <- gwindow("Add widgets example")
g <- ggroup(cont = w, horizontal=FALSE)
glabel("An example of adding widgets dynamically to a GUI", cont = g)
glabel("Set a non blank value for the combobox and a new one is added", cont = g)
gbutton("click to see stored value(s)", cont = g, handler = function(h,...) {
  out <- sapply(l, svalue)
  galert(paste(out, collapse="  "), parent=w)
})

## we keep this as a global and use <<- within the handler
l <- list()
addCombobox <- function(g) {
  g1 <- ggroup(cont = g)
  glabel("label", cont = g1)
  l[[length(l) + 1]] <<- gcombobox(c("",letters), cont = g1, handler = function(h,...) {
    out <- sapply(l, svalue)
    if(!any(sapply(out, function(i) i == "")))
      addCombobox(g)
  })
}

addCombobox(g)


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
