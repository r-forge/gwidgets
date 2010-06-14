w <- gwindow("An example form")
g <- ggroup(horiz=FALSE, cont = w)

lst <- list(type = "ggroup",
            horizontal=FALSE,
            children = list(
              list(type="fieldset",
                   label = "argument",
                   width = 500,
                   children=list(
                     list(type="gcombobox",
                          name="Combo",
                          label = "combo",
                          items=letters),
                     list(type = "gslider",
                          name="slider",
                          label="slider"),
                     list(type = "gedit",
                          name="edit",
                          label = "edit",
                          text = "edit this",
                          width=200)
                     )
                   )
              )
            )


f <- gformlayout(lst, cont = g)

bg <- ggroup(cont = g)

gbutton("ok", cont = bg, handler = function(h,...) {
  vals <- svalue(f)
  out <- paste(capture.output(str(vals)), collapse="<br>")
#  out <- paste(names(vals),unlist(vals), sep="=", collapse="<br>")
  galert(out)
  
})

## show top level
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE

