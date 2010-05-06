## test of ggroup arguments expand, fill, anchor
library(gWidgets)
options(guiToolkit="Qt")

g <- ggroup(cont=gwindow())

gbutton("nothing", cont=g)
gbutton("fillboth", expand=TRUE, fill="both", cont=g)
gbutton("fillx", expand=TRUE, fill="x", cont=g)
gbutton("filly", expand=TRUE, fill="y", cont=g)
gbutton("anchor NW", expand=TRUE, anchor=c(-1,1), cont=g)
gbutton("anchor SE", expand=TRUE, anchor=c(1,-1),cont=g)


            
