w = gwindow("Examples of gprocessing widget")
g = ggroup(cont = w, horizontal=FALSE)

if(!gWidgetsWWWIsLocal()) {
  ghtml("gprocessing only works with local installs", cont = g)
} else {
ghtml("Example of gprocessing widget. The gsvg and gcanvas widgets are better suited for graphics, but this gives some low-level flexibility and reasonable interactivity with the local server.", cont = g)

f <- gframe("Basic example showing histogram", container = g)

## a histogram
p = gprocessingjs(width=500, height = 400, container  = f)
p$Hist = function(.,x, breaks = "Sturges", col = "goldenrod") {
  out = hist(x, breaks = breaks, plot=FALSE)
  p$plot.new()
  p$plot.window(xlim = range(out$breaks), ylim = c(0, max(out$counts)))
  p$axis(1)
  p$title(main = deparse(substitute(x)))
  nb = length(out$breaks)
  p$rect(out$breaks[1:(nb-1)], rep(0, length=nb-1), out$breaks[2:nb],
    out$counts, col)
}
p$Hist(faithful$eruptions)
gslider(from = 1, to = length(faithful$eruptions), by = 1, cont = g, handler =
  function(h,...) { p$Hist(faithful$eruptions, breaks = as.numeric(svalue(h$obj))) })


f <- gframe("Example of interacting with handler", horiz=FALSE, cont = g)
## Make regression line that follows mouse clicks
x = c(); y = c()
glabel("Click in graph to set points", container=f)
p1 = gprocessingjs(width=500, height = 400, container  = f)
p1$..background = 200
p1$plot.new()
p1$plot.window(xlim=c(0,10), ylim=c(0,10))
addHandlerMouseclick(p1, handler = function(h,...) {
  xy = p1$pixelsToXY(h$xy) ## need to convert to user coordinates
  assign("x",c(x, xy[1]), envir=.GlobalEnv)
  assign("y",c(y, xy[2]), envir=.GlobalEnv)
  p1$plot.new() # paints background
  p1$points(x, y, cex=2, col="red")
  if(length(x) != 1) {
    res = lm(y ~ x)
    p1$abline(res)
 }
})

## example of idle handler
f <- gframe("A clock. Updates every 5 seconds", cont = g)
p2 <- gprocessingjs(width=400, height=400, cont = f)
p2$plot.new()
p2$plot.window(xlim=c(-1,1), ylim=c(-1,1))
p2$makeClock <- function(., ...) {
  p2$plot.new()
  t <- seq(0,2*pi, length=50)
  p2$polygon(cos(t), sin(t), col=100)
  curTime <- as.POSIXlt(Sys.time())
  secA <- pi/2 - curTime$sec/60*2*pi
  minA <- pi/2 - curTime$min/60*2*pi
  hrA <-  pi/2 - curTime$hour/12*2*pi

  hrH= .6; minH = .8; secH <- .9
  p2$lines(hrH*c(0,cos(hrA)), hrH*c(0,sin(hrA)), lwd=8, col="red")
  p2$lines(minH*c(0,cos(minA)), minH*c(0,sin(minA)), lwd=2, col="blue")
  p2$lines(secH*c(0,cos(secA)), secH*c(0,sin(secA)))
}
## 1000 -- 1 second
addHandlerIdle(p, handler = function(h,...) p2$makeClock(), interval = 5*1000)
p2$makeClock()

f <- gframe("Example of some methods", cont = g)
p3 = gprocessingjs(width=400,height = 400, container = f)
p3$background(250) 
p3$axis(1)
p3$title("Title", sub="subtitle", xlab="xlab", ylab="ylab")
p3$plot.window(xlim=c(0,10), ylim= c(0,5))
p3$polygon(c(0,5,5), c(0,0,3), col="red")
p3$polygon(c(5,10,10), c(5,5,3), col=240) # gray scale 0 to 256
p3$points(x = 1:5, y = 1:5)
p3$lines(x = 1:5, y = 1:5, col=200)
for(i in 1:5)
  p3$text(i,5-i,"text goes here", col="blue", cex=1 + i/5)
}
## save for calling html file
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
