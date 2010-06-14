w <- gwindow("Test of different layouts")
width <- 500
## ggroup
## ggroup horizontal works -- if -- we specify size for frames, ...
g <- gframe("Test of layouts: ggroup, gframe, gexpandgroup, gnotebook and glayout", cont=w, horizontal=FALSE)

fg <- gframe("horizontal ggroup", cont=g)
# horizontal group
f <- ggroup(cont=fg, horizontal = TRUE, width=width)
b <- gbutton("button1", cont=f)
addHandlerClicked(b, handler = function(h,...) gmessage("clicked",parent=b))
b1 <- gbutton("button 2", cont=f)


## frame
f1 <- gframe("gframe container with title",cont=g, horizontal=TRUE, width=width)
l <- glabel("buttong", cont=f1)
font(l) <- c("font-weight"="bold")
b1 <- gbutton("button 3", cont=f1)

## gexpandgroup
f <- gexpandgroup("gexpandgroup: click bar to toggle visibility",cont=g, horizontal=TRUE, width=width)
b <- gbutton("button 4", cont=f)
b1 <- gbutton("button 5", cont=f)

## gnotebook
f <- gframe("gnotebook container", cont = g)
f <- gnotebook(cont=f,horizontal=TRUE)
size(f) <- c(width,200)
gbutton("button", cont = f, label = "tab 1")
gbutton("Another button", cont = f, label = "tab 2")
## tooltips when tabpos not on bottom (=1)
gbutton("button with tooltip", cont=f, label="one",tooltip="hover")


## glayout
fg <- gframe("glayout example", cont = g)
f <- glayout(cont = fg)
f[1,1] <- (tb = gbutton("hi", cont=f))
f[1,2:3,anchor=c(1,0)] <- (tl = glabel("label",cont=f))
## set fonts
font(tl) <- c("weight" = "bold")
f[2,2] <- glabel("missing 2,1 in this row", cont=f)
addHandlerClicked(tb,handler=function(h,...) svalue(tl) = "new label")

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
