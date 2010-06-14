w <- gwindow("Windows example")
g <- ggroup(cont = w, horizontal = FALSE)
ghtml(paste("Illustration of state of modal dialogs and subwindows. Modal dialogs",
            "are used with a handler (gconfirm).",
            "Subwindows are created by gwindow with the argument parent=toplevel_window.",
            sep=" "), cont = g)

g1 <- gexpandgroup("Quick message dialog",cont=g)
b1 <- gbutton("galert", cont=g1, handler = function(h,...) {
  galert("for quick transient messages", title="galert dialog")
})


g1 <- gexpandgroup("Modal dialogs.",cont=g)

b2 <- gbutton("gmessage", cont=g1, handler = function(h,...) {
  ## parent needed -- dialog animation comes from parent.
  gmessage("hi there", parent = b2)
})

b3 <- gbutton("gconfirm", cont=g1, handler = function(h,...) {
  gconfirm("gconfirm handler is run on ok but not cancel.", parent = b3,
           handler = function(h,...) {
             galert("you clicked ok", parent=b3)
           })
})

b4 <- gbutton("ginput", cont=g1, handler = function(h,...) {
  ginput("ginput: input returned to handler via h$input",
         parent = b4, handler = function(h,...) {
           galert(h$input)
         })
})


g1 <- gexpandgroup("A subwindow allowing interactivity",cont=g)


b5 <- gbutton("gwindow" ,cont=g1, handler = function(h,...) {
  w1 <- gwindow("subwindow", parent=w)
  g1 <- ggroup(cont = w1, horizontal=FALSE)
  ## need the gimage code first!!!
  glabel("subwindow text", cont = g1)
  gseparator(cont = g1)
  gbutton("dismiss", cont = g1, handler = function(h,...) dispose(w1))
  visible(w1) <- TRUE #show
})


gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
