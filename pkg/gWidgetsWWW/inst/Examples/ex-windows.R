w <- gwindow("Windows example")
g <- ggroup(cont = w, horizontal = FALSE)
ghtml(paste("Illustration of state of modal dialogs and subwindows.",
            "<br />",
            "Modal dialogs",
            "are used with a handler (gconfirm).",
            "<br />",
            "Subwindows are created by gwindow with the argument <code>parent=toplevel_window</code>.",
            sep=" "),
      cont = g)

g1 <- gexpandgroup("Quick message dialog",cont=g)
b1 <- gbutton("galert", cont=g1, handler = function(h,...) {
  galert("for quick transient messages", title="galert dialog", parent=w)
})


g1 <- gexpandgroup("Modal dialogs.",cont=g)

b2 <- gbutton("gmessage", cont=g1, handler = function(h,...) {
  ## parent needed -- dialog animation comes from parent.
  gmessage("hi there", parent = b2)
})

b3 <- gbutton("gconfirm", cont=g1, handler = function(h,...) {
  gconfirm("gconfirm handler is run on ok but not cancel.", parent = b3,
           handler = function(h,...) {
             galert("you clicked ok",parent=w)
           })
})

b4 <- gbutton("ginput", cont=g1, handler = function(h,...) {
  ginput("ginput: input returned to handler via h$input",
         parent = b4, handler = function(h,...) {
           galert(h$input, parent=w)
         })
})


g1 <- gexpandgroup("A subwindow allowing interactivity",cont=g)


b5 <- gbutton("gwindow" ,cont=g1, handler = function(h,...) {
  w1 <- gwindow("subwindow", parent=w)
  g1 <- ggroup(cont = w1, horizontal=FALSE)
  glabel("Lorem ipsum...", cont = g1)
  gseparator(cont = g1)
  gbutton("dismiss", cont = g1, handler = function(h,...) dispose(w1))
  visible(w1) <- TRUE #show
})


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
