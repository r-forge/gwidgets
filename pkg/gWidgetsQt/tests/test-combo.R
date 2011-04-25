library(gWidgets)
options(guiToolkit="Qt")

if(gWidgets:::.bypassRequire("testthat")) {
  is.nullna <- function(x) is.null(x) || is.na(x)

  w <- gwindow()
  x <- 1:3
  widget <- gcombobox(x, selected=0, cont=w)
  
  
  ## length
  expect_that(length(widget) == length(x), is_true())
  
  ## svalue
  expect_that(is.nullna(svalue(widget)), is_true())
  
  svalue(widget,ind=TRUE) <- 1
  
  expect_that(svalue(widget) == x[1], is_true())
  expect_that(svalue(widget, index=TRUE)==1, is_true())
  
  ## svalue<-
  svalue(widget,index=TRUE) <- 3; 
  expect_that(svalue(widget, index=TRUE) == 3, is_true())
  
  ## []
  expect_that(length(widget[]) == length(x), is_true())
  
  ##[]<-
  x <- letters
  widget[] <- x
  expect_that(length(widget[]) == length(x), is_true())
  svalue(widget, index=TRUE) <- 2
  expect_that(svalue(widget) == x[2], is_true())
  
  ## handler
  addHandlerClicked(widget, handler=function(h,...) {
    print("clicked once")
    print(svalue(h$obj))
  })

}
