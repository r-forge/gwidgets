require(gWidgets)
options("guiToolkit"="rJava")

files <- list.files(system.file("tests",package="gWidgets"),
                    pattern = "\\.R$",
                    full.names = TRUE)


for(unitTest in files) {
  source(unitTest)
}
