library(gWidgets)
options(guiToolkit="Qt")


w <- gwindow("Layout test")

tbl <- glayout(cont=w)

tbl[1,1] <- "a label"
tbl[2,1] <- "no arguments"
tbl[3,1, expand=TRUE] <- gbutton("expand = TRUE", cont=tbl)
tbl[4,1, expand=TRUE, anchor=c(-1,1)] <- gbutton("expand = TRUE, anchor=c(-1,1)", cont=tbl)
tbl[5,1, expand=TRUE, anchor=c(1,-1)] <- gbutton("expand = TRUE, anchor=c(1,-1)", cont=tbl)
tbl[6,1, expand=TRUE, fill="x"] <- gbutton("expand = TRUE, fill='x'", cont=tbl)
tbl[7,1, expand=TRUE, fill="y"] <- gbutton("expand = TRUE, fill='y'", cont=tbl)

size(tbl) <- c(600,600)
