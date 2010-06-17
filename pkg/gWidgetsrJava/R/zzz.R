.onLoad <- function(libname,pkgname,...) {
  require(methods)
  require(rJava)

  ## we supply our own JavaGD class
    Sys.setenv("JAVAGD_CLASS_NAME"="org/rosuda/JGR/toolkit/JavaGD")  

  ## for rJava post 0.5.0 we use .jpackage
  ## and .jengine to get callback into R
  .jpackage(pkgname)
  .jengine(TRUE)


  ##   .jinit(c(system.file(paste("java","gWidgetsrJava.jar",
  ##                              sep=.Platform$file.sep),
  ##                        package="gWidgetsrJava"),
  ##            system.file(paste("jri","JRI.jar",
  ##                              sep=.Platform$file.sep),
##                                    package="rJava"))
  ##          )
}
         

       

.onAttach <- function(...) {
   #  loadGWidgetIcons()
}
