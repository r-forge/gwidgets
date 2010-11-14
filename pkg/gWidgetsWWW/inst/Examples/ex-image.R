## An illustration of how gimage can be used for a graphics
## needs a png driver

## need w to be global!
w <- gwindow("")

##' make an error page if any issues
##' @param msg a message for the error
makeErrorPage <- function(msg="") {
  svalue(w) <- "Error"
  ghtml(sprintf("Error: %s", msg), cont=w)
}

## sanity checks
if(!capabilities()['png']) {
  makeErrorPage("This demo needs a png driver to work")
} else if(!require(lattice, quietly=TRUE, warn=FALSE)) {
  makeErrorPage("This demo uses the lattice package")
} else {

  ## function to draw graphic
  makePlot <- function(fileName, width, height) {
    library(lattice)
    width <- 500; height <- 300
    png(file=fileName, width=width, height=height)
    print(histogram(rnorm(100)))
    dev.off()
  }

  ## static file for writing to / reading from
  fileName <- getStaticTmpFile(ext=".png")

  
  
  svalue(w) <- "Using Cairo plus gimage"
  g <- ggroup(horizontal = FALSE, cont = w)
  b <- gbutton("Make new plot", cont = g, handler = function(h,...) {
    if(file.exists(fileName))
      unlink(fileName)
    fileName <<- getStaticTmpFile(ext="png")
    makePlot(fileName, width,height)
    svalue(img) <- convertStaticFileToUrl(fileName)
  })
  
  makePlot(fileName, width, height)
  img <- gimage(convertStaticFileToUrl(fileName), cont = g)
  
  gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
}

visible(w) <- TRUE
