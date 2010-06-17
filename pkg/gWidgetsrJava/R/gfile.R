## file chooser dialog: creates gfile and gfilebrowser
setMethod(".gfile",
          signature(toolkit="guiWidgetsToolkitrJava"),
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
            multiple <- getWithDefault(args$multiple, FALSE)
            
            
            type = match.arg(type)
            availTypes = c(
              "open"="open",
              "save"="save",
              "selectdir"="select-folder",
              "createdir"="create-folder"
              )
            

            ## this is basic one
            chooser = .jnew("javax/swing/JFileChooser",as.character(getwd()))

            ## fix up: filter
            if(!is.null(filter))
              gwCat("gfile: implement filtering\n")

            ## multiple?
            if(multiple)
              chooser$setMultiSelectionMEnabled(multiple)
            
            ## directories only
            if(type == "selectdir")
              chooser$setFileSelectionMode(.jfield(chooser,name="DIRECTORIES_ONLY"))
              
            
            ## open dialog -- its modal
            ## null is NULL instnace
            null = .jnull("java/awt/Component")
            retval = switch(type,
              open=.jcall(chooser,"I","showOpenDialog",null),
              save=.jcall(chooser,"I","showSaveDialog",null),
              selectdir = .jcall(chooser,"I","showOpenDialog",null)
              )

            
            ## dialog closing handled by java
            if (retval == .jfield(chooser,name="APPROVE_OPTION")) {
              ## file selected
              if(!multiple) {
                selfile = chooser$getSelectedFile()
                filename = selfile$getPath()
              } else {
                selfiles <- chooser$getSelectedFiles()
                filename <- sapply(selfiles, function(i) i$getPath())
              }
              h = list(obj = chooser,action=action, file=filename)
              if(!is.null(handler)) 
                handler(h)

              ## how to return filename?
              return(filename)
            } else if(retval == .jfield(chooser,name="CANCEL_OPTION")) {
              ## cancel

            }

            

            
          })


##################################################
## gfilebrowser is not modal, like gfile
setClass("gFilebrowserJava",
         contains="gEditrJava",
         prototype=prototype(new("gEditrJava"))
         )


## create a browse button -- put value into text box
setMethod(".gfilebrowse",
          signature(toolkit="guiWidgetsToolkitrJava"),
          function(toolkit,
                   text="Select a file...", type="open",  quote=TRUE,
                   container=NULL, ...) {

            theArgs = list(...)
            contArgs = list(horizontal=TRUE, container=container)
            if(!is.null(theArgs$expand)) contArgs$expand = theArgs$expand
            if(!is.null(theArgs$anchor)) contArgs$anchor = theArgs$anchor
            if(!is.null(theArgs$label)) contArgs$anchor = theArgs$label
            
            group = do.call("ggroup", contArgs)
            entry = gedit(text=text, width=20, container=group, ...)
            browseButton = gbutton("browse",container=group)

            file.cb = function(h,...) {
              ## called when button is clicked
              
              ## in this h is gFile object, not gBrowse object
              gfile(text=text,
                    type = type,
                    handler = function(h,...) svalue(entry) <- h$file,
                    quote = TRUE
                    )
            }
            addhandlerclicked(browseButton,handler=file.cb)


            ## put entry as widget to pick up gEdit methods
            obj = new("gFilebrowserJava",
              block=group, widget=entry@widget@widget,
              toolkit=toolkit,ID=getNewID(),  e = new.env())

            invisible(obj)
          })

