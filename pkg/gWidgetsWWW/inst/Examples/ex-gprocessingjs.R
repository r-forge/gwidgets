
w = gwindow("Examples of gprocessing widget")
g = ggroup(cont = w, horizontal=FALSE)

con <- textConnection("
The <b>gprocessingjs</b> widget uses the processingjs libraries (processingjs.org) to implement a graphic area that has more interactivity than that provided by <b>gsvg</b> or <b>gcanvas</b>. The cost is that only some low-level graphic primitives are implemented. Even there, they require a slightly different calling syntax. Click the source buttons below to see.
")

ghtml(readLines(con, warn=FALSE), cont=g)
close(con)

con <- textConnection('
## a histogram
f <- gframe("Basic example showing histogram", horizontal=FALSE, container = g)
p <- gprocessingjs(width=500, height = 400, container  = f)
Hist <- function(p,x, breaks = "Sturges", col = "goldenrod") {
  out <- hist(x, breaks = breaks, plot=FALSE)
  p$plot.new()
  p$plot.window(xlim = range(out$breaks), ylim = c(0, max(out$counts)))
  p$axis(1)
  p$title(main = deparse(substitute(x)))
  nb <- length(out$breaks)
  p$rect(out$breaks[1:(nb-1)], rep(0, length=nb-1), out$breaks[2:nb],
    out$counts, col)
}
Hist(p, faithful$eruptions)
gslider(from = 5, to = length(faithful$eruptions), by = 10, cont = f, handler =
  function(h,...) { Hist(p, faithful$eruptions, breaks = as.numeric(svalue(h$obj))) })
')
h1 <- readLines(con)
close(con)
eval(parse(text=paste(h1, collapse="\n")))
gbutton("source", cont=f, action=paste(h1, collapse="<br />"), handler=function(h,...) {
  w1 <- gwindow("Source of histogram", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  ghtml(h$action, cont=g)
  gseparator(cont=g)
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
})


con <- textConnection('
f <- gframe("Example of interacting with handler", horiz=FALSE, cont = g)
## Make regression line that follows mouse clicks
x <- c(); y <- c()
glabel("Click in graph to set points. A regression line is fit", container=f)
p1 = gprocessingjs(width=500, height = 400, container  = f)
p1$..background = 200
p1$plot.new()
p1$plot.window(xlim=c(0,10), ylim=c(0,10))
addHandlerMouseclick(p1, handler = function(h,...) {
 xy <- p1$pixelsToXY(h$xy) ## need to convert to user coordinates
 x <<- c(x, xy[1])
 y <<- c(y, xy[2])
  p1$plot.new() # paints background
  p1$points(x, y, cex=2, col="red")
  if(length(x) != 1) {
    res = lm(y ~ x)
    p1$abline(res)
 }
})
')
h1 <- readLines(con)
close(con)
eval(parse(text=paste(h1, collapse="\n")))
  
gbutton("source", cont=f, action=paste(h1, collapse="<br />"), handler=function(h,...) {
  w1 <- gwindow("Source of interaction example", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  ghtml(h$action, cont=g)
  gseparator(cont=g)
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
})


con <- textConnection('
## example of idle handler
f <- gframe("A clock. Updates every 5 seconds", horizontal=FALSE, cont = g)
p2 <- gprocessingjs(width=400, height=400, cont = f)
p2$plot.new()
p2$plot.window(xlim=c(-1,1), ylim=c(-1,1))
makeClock <- function(p, ...) {
  p$plot.new()
  t <- seq(0,2*pi, length=50)
  p$polygon(cos(t), sin(t), col=100)
  curTime <- as.POSIXlt(Sys.time())
  secA <- pi/2 - curTime$sec/60*2*pi
  minA <- pi/2 - curTime$min/60*2*pi
  hrA <-  pi/2 - curTime$hour/12*2*pi

  hrH <- .6; minH <- .8; secH <- .9
  p$lines(hrH*c(0,cos(hrA)), hrH*c(0,sin(hrA)), lwd=8, col="red")
  p$lines(minH*c(0,cos(minA)), minH*c(0,sin(minA)), lwd=2, col="blue")
  p$lines(secH*c(0,cos(secA)), secH*c(0,sin(secA)))
}
## 1000 -- 1 second
gbutton("Click to turn on clock. (Causes screen to flash every 5 seconds)", cont=f, handler=function(...) {
addHandlerIdle(p, handler = function(h,...) makeClock(p2), interval = 5*1000)
})
makeClock(p2)
')

h1 <- readLines(con)
close(con)
eval(parse(text=paste(h1, collapse="\n")))
gbutton("source", cont=f, action=paste(h1, collapse="<br />"), handler=function(h,...) {
  w1 <- gwindow("Source of clock example", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  ghtml(h$action, cont=g)
  gseparator(cont=g)
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
})

con <- textConnection('
f <- gframe("Example of some methods", horizontal=FALSE, cont = g)
p3 <- gprocessingjs(width=400,height = 400, container = f)
p3$background(250) 
p3$axis(1)
p3$title("Title", sub="subtitle", xlab="xlab", ylab="ylab")
p3$plot.window(xlim=c(0,10), ylim= c(0,5))
p3$polygon(c(0,5,5), c(0,0,3), col="red")
p3$polygon(c(5,10,10), c(5,5,3), col=240) # gray scale 0 to 256
p3$points(x = 1:5, y = 1:5)
p3$lines(x = 1:5, y = 1:5, col=200)
for(i in 1:5) {
  p3$text(i,5-i,"text goes here", col="blue", cex=1 + i/5)
}
')

h1 <- readLines(con)
close(con)
eval(parse(text=paste(h1, collapse="\n")))
gbutton("source", cont=f, action=paste(h1, collapse="<br />"), handler=function(h,...) {
  w1 <- gwindow("Source of example of graphic methods", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  ghtml(h$action, cont=g)
  gseparator(cont=g)
  gbutton("dismiss", cont=g, handler=function(...) dispose(w1))
  visible(w1) <- TRUE
})

## save for calling html file
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
