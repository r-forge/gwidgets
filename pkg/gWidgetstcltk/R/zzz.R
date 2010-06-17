.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(tcltk)

  ## version check
  if(as.numeric(.Tcl("info tclversion")) < 8.5) {
    cat("\n\n *** gWidgetstcltk needs tcl/tk version 8.5 or newer ***\n\n")
  }
  
  oldClasses =c("tkwin")
  setClass("tcltkObject")
  sapply(oldClasses, function(i) {
    setOldClass(i)
    setIs(i,"tcltkObject")
  })

  ## some configuration
  .Tcl("option add *tearOff 0")         # disable tearoff menus

  ## load in tcl packages
  tcl("source",system.file("tcl","autoscroll.tcl",package="gWidgetstcltk"))
  tclRequire("autoscroll")
  tcl("namespace","import","::autoscroll::autoscroll")

  ## genearte stock icons
  loadGWidgetIcons()
}
         

       

.onAttach <- function(...) {
   #  loadGWidgetIcons()
}
