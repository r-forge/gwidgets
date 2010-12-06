fn <- "gtable"
##################################################
w <- gwindow(sprintf("test %s", fn))

g <- ggroup(cont=w, horizontal=FALSE)

df <- data.frame(c=state.name[1:5], b=rnorm(1:5), a=1:5,  d=c(T,T,T,F,F), stringsAsFactors=FALSE)

widget <- gtable(df, cont=g)
size(widget) <- c(600, 200)

## handlers
## changed -- single click
addHandlerChanged(widget, handler=function(h,...) {
  galert(svalue(h$obj), parent=w)
})

## double click
addHandlerDoubleclick(widget, handler=function(h,...) {
  galert("Double click", parent=w)
})
## methods

## svalue;
gbutton("click to get svalue", cont=g, handler=function(h,..) {
  galert(svalue(widget), parent=w)
})

gbutton("click to get svalue, index=TRUE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=TRUE), parent=w)
})

gbutton("click to get svalue, index=FALSE", cont=g, handler=function(h,..) {
  galert(svalue(widget, index=FALSE), parent=w)
})

## svalue<-
gbutton("click to set svalue<- ", cont=g, handler=function(h,..) {
  svalue(widget) <- "California"
})

gbutton("click to set svalue<- by index ", cont=g, handler=function(h,..) {
  svalue(widget, index=TRUE) <- 1
})



## [
gbutton("click to get [", cont=g, handler=function(h,..) {
  galert(paste(capture.output(widget[]), collapse="<br />"), parent=w)
})


##[<-
gbutton("click to set [<- ", cont=g, handler=function(h,..) {
  widget[] <- df[1:3,]                  # shorten
})

## filter
gbutton("click to filter by '^A'", cont=g, handler=function(h,...) {
  widget$filter("c", "^A")
})

## visible<-
gbutton("click to set visible<-", cont=g, handler=function(h,...) {
  visible(widget) <- widget[,"d", drop=TRUE]
})

gseparator(cont=g)


## Test for an empty table
widget1 <- gtable(df[numeric(0),], cont=g)

## [<-
gbutton("click to populate [<-", cont=g, handler=function(h,...) {
  widget1[] <- df
})

gseparator(cont=g)
glabel("Adjust layout of columns using ..scripts", cont=g)
widget2 <- gtable(df, cont=g)
size(widget2) <- c(600, 200)
widget2$..scripts <- function(.) {
  paste("gtableNumeric = function(val) { ",
        "val = Ext.util.Format.number(val, \"?0,0.000?\");",
        "return '<span  style=\"text-align:right; color:blue\"><h1>' + val + '</h1></span>';",
        "}",
        sep="\n")
}
   


##################################################                  
gstatusbar(sprintf("Tests for %s", fn), cont=w)
visible(w) <- TRUE
