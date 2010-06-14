w <- gwindow("Test of gcombobox")
g <- ggroup(cont=w, horizontal=FALSE)

## default handler shows value of action argument
handler <- function(h,...) gmessage(svalue(h$action), parent=g)

m <- data.frame(
  values = state.name,
  icons = rep("images/home.gif",50),
  gtip = paste(state.abb,"has",state.area,"square miles.", sep=" ")
  )


g1 <- gexpandgroup("Basic combo: one column", cont=g)
gcombobox(m[,1,drop=TRUE], cont = g1)

g1 <- gexpandgroup("Combo with icon", cont=g)
gcombobox(m[,1:2], cont = g1)

g1 <- gexpandgroup("Combo with icon and tooltip", cont=g)
gcombobox(m[,1:3], cont = g1)

g1 <- gexpandgroup("Combo to replace gedit when predefined values are set", cont=g)
cb <- gcombobox(m[,1], editable = TRUE, cont = g1)
cb$..hideTrigger <- TRUE                # hide the trigger to make it look right

gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE

