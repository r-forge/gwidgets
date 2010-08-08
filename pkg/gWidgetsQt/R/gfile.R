##  Copyright (C) 2010 John Verzani
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/

## file chooser dialog: creates gfile and gfilebrowser
setMethod(".gfile",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="",
                   type=c("open","save","selectdir"),
                   initialfilename = NULL,
                   filter =  list(
                     "All files"=list(
                       patterns=c("*")
                       ),
                     "R files"=list(
                       patterns=c("*.R","*.Rdata")
                       ),
                     "text files"=list(
                       mime.types=c("text/plain")
                       )
                     ),
                   handler = NULL,
                   action = NULL,                     # 
                   ...
                   ) {
            
            force(toolkit)
            
            args = list(...)
            
            type = match.arg(type)


            fm <- Qt$QFileDialog()

            ## different things depending on type
            if(type == "open") {

              filters <- c()
              if(!is.null(filter)) {
                for(i in names(filter)) {
                  if(!is.null(filters[[i]]$pattern)) {
                    filters <- c(filters, paste(i, " (", paste(filters[[i]]$patterns, collapse=" "),
                                                ")", sep=""))
                  }
                  ## no mime.types
                }
                out <- sapply(filters, function(i) is.null(i$mime.types))
                if(any(out))
                  XXX("No filtering of mime types, only patterns")
              }

              if(length(filters) == 0)
                filters <- c("All files (*.*)")

              theFilter <- paste(filters, collapse=";;")
              
              ## how to set Title
              fm$setNameFilter(theFilter)
              fm$setDirectory(getwd())
              if(!is.null(initialfilename))
                fm$selectFile(basename(initialfilename))

            } else if(type == "save") {

              if(!is.null(initialfilename))
                fm$selectFile(basename(initialfilename))
              fm$setConfirmOverwrite(TRUE)
              fm$setFileMode(Qt$QFileDialog$AnyFile)
              
            } else if(type == "selectdir") {

              fm$setConfirmOverwrite(TRUE)
              fm$setFileMode(Qt$QFileDialog$Directory)
              fm$setOption(Qt$QFileDialog$ShowDirsOnly, TRUE)   # directory only
            }

            ret <- fm$exec()

            if(ret == 1) {
              val <- fm$selectedFiles()
              ## file selected
              if(!is.null(handler)) {
                h = list(ref = NULL, action=action, file=val)
                handler(h)
              }
              ## how to return filename?
              return(val)
            } else {
              ## cancel
              return(NA)
            }
            
            
          })


##################################################
## gfilebrowser is not modal, like gfile
setClass("gFilebrowseQt",
         contains="gEditQt",
         prototype=prototype(new("gEditQt"))
         )


## create a browse button -- put value into text box
setMethod(".gfilebrowse",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   text="Select a file...", type="open",  quote=TRUE,
                   container=NULL, ...) {

            if(is(container,"logical") && container)
              container = gwindow()
            if(!is(container,"guiWidget")) {
              warning("Container is not correct. No NULL containers possible\n" )
              return()
            }


            
            group = ggroup(horizontal=TRUE, container=container)
            entry = gedit(text=text, container=group, ...)
            browseButton = gbutton("browse",container=group)

            file.cb = function(h,...) {
              ## called when button is clicked
              
              ## in this h is gFile object, not gBrowse object
              gfile(text = text,
                    type = type,
                    handler = function(h,...) svalue(entry) <- h$file,
                    quote = TRUE
                    )
            }
            addhandlerclicked(browseButton,handler=file.cb)


            ## put entry as widget to pick up gEdit methods
            obj = new("gFilebrowseQt",
              block=group, widget=getWidget(entry),
              toolkit=toolkit, ID=getNewID(), e = new.env())

            tag(obj,"entry") <- entry
            
            invisible(obj)
          })


setMethod(".svalue",
          signature(toolkit="guiWidgetsToolkitQt",obj="gFilebrowseQt"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            entry = tag(obj,"entry")
            svalue(entry,index,drop,...)
          })

## svalue<-
setReplaceMethod(".svalue",
                 signature(toolkit="guiWidgetsToolkitQt",
                           obj="gFilebrowseQt"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   entry = tag(obj,"entry")
                   svalue(entry, index,...) <- value
                   return(obj)
          })

