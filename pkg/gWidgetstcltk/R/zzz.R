.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(tcltk)

  ## version check
  if(as.numeric(.Tcl("info tclversion")) < 8.5) {
    stop("\n\n *** gWidgetstcltk needs tcl/tk version 8.5 or newer ***\n\n")
  }
  
  oldClasses =c("tkwin")
  setClass("tcltkObject")
  lapply(oldClasses, function(i) {
    setOldClass(i)
    setIs(i,"tcltkObject")
  })

  ## some configuration
  .Tcl("option add *tearOff 0")         # disable tearoff menus

  
  ## load in tcl packages
  ## from tcltk2 we have this
  tclRequire("autoscroll")
#  tcl("source",system.file("tcl","autoscroll.tcl",package="gWidgetstcltk"))
#  tcl("namespace","import","::autoscroll::autoscroll")

  ## genearte stock icons
  loadGWidgetIcons()

  ## use.table options
  tkimage.create("photo", "::image::off", file=system.file("images", "symbol_cross.gif", package="gWidgets"))
  tkimage.create("photo", "::image::on",  file=system.file("images", "symbol_plus.gif",  package="gWidgets"))

}
         

       

.onAttach <- function(...) {
   #  loadGWidgetIcons()
}
