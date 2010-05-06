library(gWidgets)
options(guiToolkit="Qt")
if(require(testthat)) {

  
  w <- gwindow()
  tbl <- gdf(mtcars, cont=w)
  ## size
  size(tbl) <- c(400,400)
  
  ## dim
  expect_that(dim(tbl)[1]==dim(mtcars)[1], is_true())
  
  
  ## length
  expect_that(length(tbl) == dim(mtcars)[2], is_true())
  
  
  ## names
  expect_that(names(tbl)[1] == names(mtcars)[1], is_true())
  
  names(tbl)[2] <-"test"
  expect_that(names(tbl)[2] == "test", is_true())
  
  ## rownames
  expect_that(rownames(tbl)[1] == rownames(mtcars)[1], is_true())
  rownames(tbl)[2] <-"test"
  expect_that(rownames(tbl)[2] == "test", is_true())
  
  ## svalue -- returns row value
  expect_that(is.na(svalue(tbl)), is_true())
  
  svalue(tbl,ind=TRUE) <- 1
                                        #expect_that(svalue(tbl, index=TRUE)==1, is_true())
  
  ## svalue<-
                                        #expect_that({svalue(tbl,index=TRUE) <- 3; svalue(tbl, index=TRUE) == 3}, is_true())
  
  ## []
  expect_that(nrow(tbl[,]) == dim(tbl)[1], is_true())
  
  ##[]<-
  tbl[1,1] <- 25
  expect_that(tbl[1,1] == 25, is_true())
  
  
  tbl[,] <- mtcars[1:3, 1:3]
  expect_that(nrow(tbl[,]) == 3, is_true())
  expect_that(ncol(tbl[,]) == 3, is_true())
  
  
  ## visible
  x <- visible(tbl)
  x[1] <- FALSE
  visible(tbl) <- x
  
  
  ## handlers
  sapply(c("Changed", "Clicked", "Doubleclick",
           "ColumnClicked", "ColumnRightclick","ColumnDoubleclick"),
         function(i) {
           f <- function(h,...) print(h$action)
           do.call(sprintf("addHandler%s", i), list(obj=tbl, handler=f, action=i))
         })
}
