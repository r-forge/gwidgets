w <- gwindow("Example of icons with buttons")
g <- ggroup(cont = w, horizontal=FALSE)

x <- getStockIcons()
g1 <- ggroup(cont=g, horizontal=FALSE)
glabel("Buttons with stock icons in gWidgetsWWW", cont=g1)
for(i in names(x))
  gbutton(i, cont=g1)


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
 visible(w) <- TRUE
