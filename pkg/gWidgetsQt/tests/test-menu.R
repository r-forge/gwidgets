library(gWidgets)
options(guiToolkit="Qt")
if(require(testthat)) {

  makeActions <- function(nested=FALSE) {
    f <- function(...) print("hi")
    if(nested) {
      l <- list(a=gaction("one", handler=f),
                b=list(
                  b1=gaction("two", handler=f),
                  b2=gaction("two.1", handler=f)
                  ),
                c = gseparator(),
                d=gaction("three", handler=f))
    } else {
      l <- list(a=gaction("one", icon="ok",  handler=f),
                b=gaction("two", icon="cancel", handler=f),
                c = gseparator(),
                d=gaction("three", icon="quit", handler=f))
    }
    l
  }

  w <- gwindow()

  tb <- gtoolbar(makeActions(), cont=w)
  mb <- gmenu(makeActions(nested=TRUE), cont=w)

  g <- ggroup(cont=w, horizontal=FALSE, expand=TRUE)
  b <- gbutton("popup", cont=g)
  b1 <- gbutton("popup.popup", cont=g)
  b2 <- gbutton("popup.3rd", cont=g)

  mb <- gmenu(makeActions(), cont = b, popup=TRUE)
  ## giving errors
##  add3rdmousepopupmenu(b1, makeActions())
##  addpopupmenu(b2, makeActions())

}
