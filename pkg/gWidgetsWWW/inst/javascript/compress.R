##' R file to run youcompress java script to make compressed javascript
##' see for details http://developer.yahoo.com/yui/compressor/
##'
##' List of files to compress
jsFiles <- c("ext.ux.canvas.js",
             "ext.ux.example.js",
             "ext.ux.form.CodeMirror.js",
             "ext.ux.form.fileuploadfield.js",
             "ext.ux.labelbox.js",
             "ext.ux.imageBox.js",
#             "ext.ux.slidertip.js",
#             "ext.ux.spinner.js",
#             "ext.ux.spinnerformfield.js",
             "ext.ux.statusbar.js",
#             "FileUploadField.js",
             "gw-gtable.js"
#             "GMapPanel.js"
             ,"processing-0.9.1.js"
             ,"processinginit.js"

             )

## name files
f <- "gWidgets.js"
fmin <- "../basehtml/gWidgetsWWW.js"

## put into one file
cat("",file=f, append=FALSE)
for(i in jsFiles)
  cat(paste(readLines(i), collapse="\n"),"\n", file=f, append=TRUE)
## run command
cmd <- sprintf("java -jar ../../../../../yuicompressor-2.4.2.jar  %s -o %s  --charset utf-8",
               f, fmin)
#cmd <- sprintf("cp %s %s", f, fmin)     # just copy, no compress
system(cmd)
## tidy up
unlink(f)
