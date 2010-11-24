## An exmaple of using the svg device
w <- gwindow("Example of svg device within gWidgetsWWW")
g <- ggroup(cont = w, horizontal=FALSE)

if(!require(RSVGTipsDevice)) {
  glabel("This example needs the RSVGTipsDevice to be installed", cont=g)
} else {

  g1 <- ggroup(cont = g)
  b <- gbutton("Click me for another", cont = g1, handler = function(h,...) {
    svalue(sg) <- makePlot()
  })
  
  
  b <- gbutton("Graph in new window", cont = g1, handler = function(h,...) {
    w1 <- gwindow("SVG Graphic with tooltips and link", width=500, height=500, parent=w)
    svd <- gsvg(expand=TRUE, cont = w1)
    require(RSVGTipsDevice, quietly=TRUE, warn=FALSE) # must require within callback
    f <- getStaticTmpFile(".svg")
    devSVGTips(f, toolTipMode=2, toolTipOpacity=.8)
    ## graph with tooltips
    plot(mpg ~ wt, mtcars, pch=NA)
    nms <- rownames(mtcars)
    for(i in 1:nrow(mtcars)) {
      ## need to add tooltip shape by shape
      setSVGShapeToolTip(title=nms[i])    # add tooltip
      setSVGShapeURL("http://www.google.com")    # some URL
      with(mtcars, points(wt[i], mpg[i], cex=2, pch=16)) # overplot
    }
    
    dev.off()
    svalue(svd) <- f
    visible(w1) <- TRUE
    
  })
  
  gseparator(cont = g)
  
  sg <- gsvg(cont = g, width=500, height=500)
  
  makePlot <- function() {
    ## load packaage quietly
    require(RSVGTipsDevice, quietly=TRUE, warn=FALSE)
    f <- getStaticTmpFile("svg")
    devSVGTips(f)
    hist(rnorm(100))
    dev.off()
    f
  }
  
  svalue(sg) <- makePlot()
}

## show off
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
