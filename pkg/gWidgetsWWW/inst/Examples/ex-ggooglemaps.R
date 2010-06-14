w <- gwindow("Test of widgets")
g <- ggroup(cont = w, horizontal = FALSE)
if(!gWidgetsWWWIsLocal()) {
  ghtml("ggooglemaps isn't configured to work with server usage", cont = g)
} else {
  glabel("How big is Central Park? Click around perimeter to see.", cont=g)
  gmap <- ggooglemaps(x = "Central Park New York, NY", title = "Central Park, New York, NY", cont = g)
  
  ## set zoom level
  gmap$setZoom(13)
  addHandlerClicked(gmap, handler = function(h,...) {
    ## add a marker
    gmap$addMarker(h$latlng)
  })

  b <- gbutton("draw polygon", cont = g, handler = function(h,...) {
    ## draw polygon using current markers
    gmap$addPolygon()
  })
}
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE



 
