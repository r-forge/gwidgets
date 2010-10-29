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
             "ext.ux.slidertip.js",
             "ext.ux.spinner.js",
             "ext.ux.spinnerformfield.js",
             "ext.ux.statusbar.js",
             "FileUploadField.js",
             "gw-gtable.js",
             "GMapPanel.js")

## name files
f <- "gWidgets.js"
fmin <- "../basehtml/gWidgetsWWW.js"

## put into one file
cat("",file=f, append=FALSE)
for(i in jsFiles)
  cat(readLines(i), sep="\n", file=f, append=TRUE)
## run command
cmd <- sprintf("java -jar ../../../../../yuicompressor-2.4.2.jar  %s -o %s  --charset utf-8",
               f, fmin)
system(cmd)
## tidy up
unlink(f)
