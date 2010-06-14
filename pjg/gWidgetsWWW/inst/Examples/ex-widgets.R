w <- gwindow("Example of gWidgetsWWW widgets")
g1 <- ggroup(cont=w)
l <- glabel("This page shows the status of several of the widgets in gWidgetsWWW.",cont=g1)

g = gexpandgroup("gmenu", cont=g1)
handler = function(h,...) gmessage("called handler", parent=g)
action = gaction(label = "hello", icon="stop", handler = handler, parent=w)
menulist = list(action = action,
          b =list(separator=TRUE),
          c = gaction(label="a label", handler = handler, parent=w),
          d = list(action = action,
                   b=list(separator=TRUE),
                   c = gaction(label="a different label", handler = handler, parent=w))
         )
mb = gmenu(menulist, cont=w)


g = gexpandgroup("glabel", cont=g1)
widget = glabel("label", cont=g)
font(widget) = c("weight" = "bold", color="red")



g = gexpandgroup("gbutton", cont=g1)
widget = gbutton("button", cont=g)




g = gexpandgroup("ghtml", cont=g1)
##widget = ghtml(asURL("ex-widgets.txt"), cont=g)
## \" preferred to ' here, as it gets properly escaped when widget is produced
widget = ghtml("Can be <b>marked</b><em>up</em> or a <a href=\"http://www.r-project.org\">url</a>",
  cont=g)

g = gexpandgroup("gcheckbox", cont=g1)
widget = gcheckbox("label", cont=g)

g = gexpandgroup("gcheckboxgroup", cont=g1)
widget = gcheckboxgroup(state.name[1:5], cont=g)

g = gexpandgroup("gradio", cont=g1)
widget = gradio(letters[1:3], cont=g)

g = gexpandgroup("gslider", cont=g1)
widget = gslider(from=0, to=100, by=1,value=50, cont=g)


g = gexpandgroup("gspinbutton -- no trigger icons show, but arrows work", cont=g1)
widget = gspinbutton(from=0, to=100, by=1,value=50, cont=g)


g = gexpandgroup("gedit", cont=g1)
widget = gedit("edit me", cont=g)


g = gexpandgroup("gtext", cont=g1)
widget = gtext("edit me", cont=g)
size(widget) = c(200,200)


g = gexpandgroup("gimage", cont=g1)
widget = gimage("http://www.r-project.org/Rlogo.jpg", cont=g)



g = gexpandgroup("gcombobox", cont=g1)
## 3 columns is max: values, icon, tooltip
m = data.frame(a = letters[1:4],
rep("images/bullet.gif",4), c= as.character(1:4))
widget = gcombobox(m, cont=g)


g = gexpandgroup("gcombobox to look gike gedit. Uses non API proto command", cont=g1)
glabel("Start typing a state name:  ",cont=g)
widget = gcombobox(state.name, cont=g, editable=TRUE, selected=0)
widget$..hideTrigger = TRUE


g = gexpandgroup("gcalendar", cont=g1)
widget = gcalendar("select a date", cont=g)

g = gexpandgroup("gfilebrowser", cont=g1)
if(gWidgetsWWWIsLocal())
  gfile("only svalue works though", cont=g)
else
  glabel("no gfilebrowse for non-local yet", cont=g)


g = gexpandgroup("gtable", cont=g1)
widget = gtable(mtcars, cont=g, multiple=TRUE)
size(widget) = c(500,250)


g = gexpandgroup("gdf", cont=g1)
widget = gdf(mtcars[1:5, 1:6], cont=g)
size(widget) <- c(300,300)

g = gexpandgroup("gtree", cont=g1)
glabel("no gtree yet", cont=g)





g = gexpandgroup("gtoolbar", cont=g1)
glabel("gtoolbar is done through gmenubar -- only one", cont=g)

g = gexpandgroup("gstatusbar", cont=g1)
widget = gstatusbar("statusbar text", cont=w)



gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE


