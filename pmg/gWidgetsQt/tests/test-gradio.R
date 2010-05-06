library(gWidgets)
options(guiToolkit="Qt")
if(require(testthat)) {

x <- letters[1:3]
w <- gwindow()
widget <- gradio(x, selectd=1, cont=w)

## size
#size(widget) <- c(400,400)


## length
expect_that(length(widget) == length(x), is_true())


## svalue
svalue(widget, index=TRUE) <- 2
expect_that(svalue(widget, index=TRUE) == 2, is_true())

svalue(widget, index=FALSE) <- "c"
expect_that(svalue(widget) == x[3], is_true())

            
## []
expect_that(length(widget[]) == length(x), is_true())

##[]<-
x <- letters[1:5]
widget[] <- x
expect_that(widget[4] == x[4], is_true())


## handlers
sapply(c("Changed"),
       function(i) {
         f <- function(h,...) print(h$action)
         do.call(sprintf("addHandler%s", i), list(obj=widget, handler=f, action=i))
       })




}
