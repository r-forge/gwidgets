w <- gwindow("combobox")
g <- ggroup(cont=w, horizontal=FALSE)

x <- state.name[1:10]

## this stuff isn't working!
icons <- rep(c("ok", "help"), 5)
tips <- toupper(x)
DF <- data.frame(x,icons, tips, stringsAsFactors=FALSE)

cb1 <- gcombobox(x, cont=g)

cb2 <- gcombobox(DF[,1], cont=g)
## issues
#cb3 <- gcombobox(DF[,1:2], cont=g)
#cb4 <- gcombobox(DF, cont=g)



## type ahead
cb5 <- gcombobox(state.name, cont=w, autocomplete=TRUE)


## editalbe
cb6 <- gcombobox(x, editable=TRUE, cont=g)

## not selected
cb7 <- gcombobox(x, selected=0, cont=g)

## tests

## svalue
expect_equal(svalue(cb1), x[1])
expect_equal(svalue(cb1, index=TRUE), 1)
expect_equal(svalue(cb7), NA)

## svalue<-
svalue(cb1) <- x[2]
expect_equal(svalue(cb1), x[2])

svalue(cb1, index=TRUE) <- 3
expect_equal(svalue(cb1), x[3])

svalue(cb6) <- "Puerto Rico"
expect_equal(svalue(cb6), "Puerto Rico")

## [
expect_equal(cb1[], x)

## [<
cb1[] <- state.name[1:25]


## length
expect_equal(length(cb1[]), 25)
