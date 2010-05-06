require(gWidgets)
options("guiToolkit"="Qt")


gWidgetsDir <- system.file("tests",package="gWidgets")
## should be there, but just in case
if(gWidgetsDir != "") {
  files <- list.files(gWidgetsDir,
                      pattern = "\\.R$",
                      full.names = TRUE)

  
  for(unitTest in files) {
    print(sprintf("Run test for %s\n=============", unitTest))
    source(unitTest)
  }
}
