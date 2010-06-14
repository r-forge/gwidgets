w <- gwindow("Example of canvas device within gWidgetsWWW")
g <- ggroup(cont = w, horizontal=FALSE)
ghtml(paste("Example of using the canvas device with gWidgetsWWW. Requires HTML5-compliand browser to work.",
            "<br>",
            "The canvas device uses javascript to write the graphic, not an image. It is an alternative to the gsvg device, which requires its image to be loaded from a file.",
            collapse="\n"),

            cont = g)

canvas <- gcanvas(cont = g, width=500, height=500)

makePlot <- function() {
  ## load packaage quietly
  require(canvas, quietly=TRUE, warn=FALSE)
  f <- paste(getStaticTmpFile(), ".canvas", sep="")
  canvas(f, width=500, height=500, bg="#ffffff") # transparent will allow overplot
  hist(rnorm(100))
  dev.off()
  f
}

svalue(canvas) <- makePlot()

b <- gbutton("click me for another", cont = g, handler = function(h,...) {
  svalue(canvas) <- makePlot()
})


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
