library(gWidgets)
options(guiToolkit="Qt")
if(require(testthat)) {


w <- gwindow()
tbl <- gtable(mtcars, cont=w)

## dim
expect_that(dim(tbl)[1]==dim(mtcars)[1], is_true())


## length
expect_that(length(tbl) == dim(mtcars)[2], is_true())

## svalue
expect_that(is.null(svalue(tbl)), is_true())

svalue(tbl,ind=TRUE) <- 1
expect_that(svalue(tbl) == 21, is_true())

expect_that(svalue(tbl, index=TRUE)==1, is_true())
expect_that(length(svalue(tbl, drop=FALSE))==length(tbl), is_true())

## svalue<-
expect_that({svalue(tbl,index=TRUE) <- 3; svalue(tbl, index=TRUE) == 3}, is_true())
            
## []
expect_that(nrow(tbl[,]) == dim(tbl)[1], is_true())

##[]<-
tbl[,] <- mtcars[1:3, 1:3]
expect_that(nrow(tbl[,]) == 3, is_true())
expect_that(ncol(tbl[,]) == 3, is_true())
svalue(tbl, index=TRUE) <- 2
expect_that(svalue(tbl) == mtcars[2,1], is_true())

## visible
x <- visible(tbl)
x[1] <- FALSE
visible(tbl) <- x

## icons as matrix
icons <- rep("ok", dim(tbl)[1])
df <- data.frame(visible=x, icon=icons, a=1:3, b=letters[1:3], stringsAsFactors=FALSE)
tbl[,] <- df


## handler
addHandlerClicked(tbl, handler=function(h,...) {
  print("clicked once")
  print(svalue(h$obj))
})

addHandlerDoubleclick(tbl, handler=function(h,...) {
  print("clicked twice")
  print(svalue(h$obj))
})

## filter
tbl <- gtable(mtcars, filter.column=2, cont=gwindow())
svalue(tbl, index=TRUE) <- 2
expect_that(svalue(tbl, index=TRUE) == 2, is_true())

}
