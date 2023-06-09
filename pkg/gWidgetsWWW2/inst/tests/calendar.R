w <- gwindow("calendar")
g <- ggroup(cont=w, horizontal=FALSE)

## default
c1 <- gcalendar(cont=g)

## initial date
c2 <- gcalendar("2000-01-31", cont=g)

## with different format
c3 <- gcalendar("01/31/2000", cont=g, format="%m/%d/%Y")

## test
expect_equal(svalue(c1), as.Date(NA))
expect_equal(svalue(c2), as.Date("2000-01-31"))
## this seems wrong
expect_equal(svalue(c3) , as.Date("01/31/2000", format=c3$date_format))


