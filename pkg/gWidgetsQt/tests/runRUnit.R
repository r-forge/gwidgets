## Run test suite through RUnit if installed
##
## RUnit is better at testing non-interactive features
## We add RUnit tests to the gWidgets/tests directory in files
## names test.XXX containing functions test-XXX <- function() {}

library(gWidgets)
options(guiToolkit="Qt")

if(gWidgets:::.bypassRequire("RUnit")) {
  
  testsuite.gWidgets <- defineTestSuite("gWidgets", 
                                        dirs = system.file("tests",package="gWidgets"),
                                        testFileRegexp = "^test-.+\\.R", 
                                        testFuncRegexp = "^test.+", 
                                        rngKind = "Marsaglia-Multicarry", 
                                        rngNormalKind = "Kinderman-Ramage")
  
  testResult <- runTestSuite(testsuite.gWidgets) 
  printTextProtocol(testResult) 
}
