## examples of tables
d <- mtcars[1:3, 1:6]


w <- gwindow("Examples of gtable widget", visible=FALSE)
g <- ggroup(cont=w, horizontal=FALSE)
glabel(svalue(w), cont=g)

## sizing
f <- gframe("Should call size method -- otherwise only one row shows!", cont=g)
gtable(d, cont=f)

## single vector
f <- gframe("Works with single column, but can feed data frame too", cont=g)
tbl <- gtable(state.name[1:3], cont=f)
size(tbl) <- c(500, 100)

## [<- method
f <- gframe("can reset values to choose from", cont=g)
tbl <- gtable(d, cont=f); size(tbl) <- c(500,200)
b <- gbutton("Click me to shorten to two rows", cont=f)
addHandlerClicked(b, handler=function(h,...) h$action[] <- d[1:2,], action=tbl)

## click handler
f <- gframe("addHandlerClick for selection click, also add handler double click", cont=g)
tbl <- gtable(d, cont=f); size(tbl) <- c(500,100)
addHandlerClicked(tbl, handler=function(h,...) {
  gmessage(sprintf("Value: %s, index: %s", svalue(h$obj), svalue(h$obj, index=TRUE)), parent=h$obj)
})

## sorting
f <- gframe("Can sort by clicking on headers. -- Try it", cont=g)
tbl <- gtable(d, cont=f); size(tbl) <- c(500,100)
addHandlerClicked(tbl, handler=function(h,...) {
  gmessage(sprintf("Value: %s, index: %s", svalue(h$obj), svalue(h$obj, index=TRUE)), parent=h$obj)
})


## filtering
f <- gframe("Filtering done by proto method filter.", cont=g)
tbl <- gtable(mtcars[, 1:5], cont=f);size(tbl) <- c(500,300)
glabel("No Cyls:", cont=f)
cb <- gcombobox(c("<All>", unique(mtcars$cyl)), cont=f, handler=function(h,...) {
  val <- svalue(h$obj)
  if(val == "<All>")
    val <- ""
  tbl$filter("cyl", val)    # clears if ""
})


##
gstatusbar("Powered by gWidgets and Rapache", cont=w)
visible(w) <- TRUE
