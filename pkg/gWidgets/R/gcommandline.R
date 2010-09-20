##' @include guiComponents.R

##' Class for a commandline widget
setClass("gCommandline",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )

##' constructor of widget for use as a command line
##'
##' @exports
gcommandline =function(
  command = "", assignto = NULL, useGUI = TRUE, useConsole = FALSE,
  prompt = getOption("prompt"), width = 500, height = 0.6 * width,
  container = NULL, ... ,
  toolkit=guiToolkit()){
  widget =  .gcommandline (toolkit,
    command=command, assignto=assignto,
    useGUI = useGUI, useConsole=useConsole,
    prompt=prompt, width=width, height=height, container=container, ...
    )
  obj = new( 'guiComponent',widget=widget,toolkit=toolkit) 
  return(obj)
}


##' generic for toolkit dispatch
##' @alias gcommandline
setGeneric( '.gcommandline' ,
           function(toolkit,
                    command = "", assignto = NULL,
                    useGUI = TRUE, useConsole = FALSE,
                    prompt = getOption("prompt"), width = 500,
                    height = 0.6 * width, container = NULL, ... )
           standardGeneric( '.gcommandline' ))


##' gcommandline implementation for any toolkit
setClass("gCommandlineANY",
         representation=representation("gComponentANY",
           editArea="guiWidgetOrNULL",
           outputArea="guiWidgetOrNULL",
           useGUI = "logical",
           useConsole = "logical"
           ),
         contains="gComponentANY",
         prototype=prototype(new("gComponentANY"))
         )


##' gcommandline constructor for ANY toolkit
##' @alias gcommandline
setMethod(".gcommandline",
          signature(toolkit="ANY"),
          function(toolkit,
                   command = "", assignto=NULL,
                   useGUI = TRUE, 
                   useConsole = FALSE,
                   prompt = getOption("prompt"),
                   width = 500, height = 0.6 * width,
                   container = NULL,
                   ...) { 

            force(toolkit)

            .history = character()

            
            ## adjust command if need be
            if(nchar(command) > 0 && !is.null(assignto))
              command = addAssignto(command, assignto)


            if(is(container,"logical") && container)
              container = gwindow("gCommandLIne")
          ##
            if(useGUI == TRUE) {
              
              
              ## the main widgets
              group = do.call("ggroup", list(horizontal=FALSE, cont=container, expand=TRUE, width=width, height=height))
              ## main objects
              bgroup <- ggroup(horizontal=TRUE, cont = group)
              pgroup = gpanedgroup(horizontal=FALSE, cont=group, expand=TRUE)

              ## toolbar
              openButton = gbutton("open",
                handler = function(h,...) {
                    gfile("Select a file to read into command line",
                          type="open",
                          handler = function(h,...) {
                            file = h$file
                            svalue(editArea) <- readLines(file)
                          })
                  }, cont = bgroup)
              saveButton = gbutton("save",
                handler = function(h,...) {
                  sw = gwindow("Save buffer contents")
                  group = ggroup(horizontal=FALSE, container=sw)
                    saveFileName = gfilebrowse("",type="save", cont=group)
                  
                  tgroup = ggroup(cont=group);
                  glabel("Save which values?", cont=tgroup)
                  saveType = gradio(c("commands","output"),horizontal=TRUE,
                    index=FALSE, cont=tgroup)
                  gseparator(container=group)
                  buttonGroup = ggroup(container=group)
                  addSpring(buttonGroup)
                    gbutton("save",handler=function(h,...) {
                      filename = svalue(saveFileName)
                      if(filename == "") {
                        cat("Need file to save to\n")
                        return()
                      }
                      if(svalue(saveType) == "commands")
                        values = svalue(editArea)
                      else
                        values = svalue(outputArea) 
                      writeLines(values, filename)
                      dispose(sw)
                    }, container=buttonGroup)
                }, cont = bgroup)
              historyButton = gbutton("history",
                handler = function(h,...) {
                  wh = gwindow("Command history")
                  gh = ggroup(horizontal=FALSE, cont=wh)
                  .history = tag(obj,"history") # obj is gcli object
                  ##                    df = data.frame(history=.history, stringsAsFactors=FALSE)
                    tbl = gtable(.history, handler=function(h,...) {
                      svalue(editArea) <- as.character(svalue(tbl,drop=TRUE))
                      dispose(wh)
                    },
                      cont=gh, expand=TRUE)
                  tg = ggroup(cont=gh); addSpring(tg)
                  gbutton("close",cont=tg,handler=function(h,...) dispose(wh))
                }, cont = bgroup)
              clearButton = gbutton("clear",
                handler = function(h,...) svalue(editArea) <- "",
                cont = bgroup
                )
              evaluateButton = gbutton("evaluate",
                handler = function(h,...) evalEditArea(obj),
                cont = bgroup)
              
              editArea = gtext("## Type commands here, then click on evaluate", container=pgroup, width=width)
              outputArea = gtext("Output:", container=pgroup, width=width)
              svalue(pgroup) <- 0.3
              
          ##            pg = gpanedgroup(outputArea, editArea, horizontal=FALSE)
          ##            add(group, pg, expand=TRUE)
          
          obj = new("gCommandlineANY",
            block=group,
                widget = group,
            toolkit=toolkit,
            ID=getNewID(),
            editArea = editArea,
            outputArea = outputArea,
            useGUI = useGUI,
            useConsole = useConsole
                )
          
          ## initialize history
          tag(obj,"history")  <- c()
        } else {
          
          obj = new("gCommandlineANY",
            block=container,
                widget = container,
                toolkit=toolkit,
                ID=getNewID(),
                editArea = container,
                outputArea = container,
                useGUI = useGUI,
                useConsole = useConsole
                )
            }

            if(nchar(command) > 0 )
              svalue(obj) <- command
            
            return(obj)
              
          })
          
## method to evaluate
## obj is returned by .gcommandline
evalEditArea = function(obj) {
  
  ## evaluate edit area, append results to outputArea
  chunk = svalue(obj@editArea)
  retVal = evalChunkReturnOutput(chunk)
  if(! retVal$error) {
    .history = tag(obj,"history")
    .history <- c(.history, chunk)
    if(length(.history) > 25) .history <- .history[2:26]
    tag(obj,"history") <- .history
  }
  add(obj@outputArea, retVal$output)
}


## taken from Sweave
## takes a chunk, interweaves command and output
evalChunkReturnOutput = function(chunk, prompt = getOption("prompt")) {

  output = ""
  addToOutput = function(...) 
    output <<- paste(output,..., sep=" ", collapse="\n")
  
  chunkexps <- try(parse(text=chunk), silent=TRUE)
  if(inherits(chunkexps,"try-error")) {
   addToOutput(chunkexps)
   cat("Houston, we have a problem with:\n",chunkexps,"\n")
   return(list(output=output, error=TRUE))
 }

  if(length(chunkexps) == 0)
    return(list(output=output, error=FALSE))

  for(nce in 1:length(chunkexps)) {
    ce <- chunkexps[[nce]]
    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
    command = paste(prompt,
      paste(dce,collapse=paste("\n", getOption("continue"), sep="")),
      sep="", collapse=""
      )

    addToOutput(command,"\n")

    ## is there output?
    tmpcon <- file()
    sink(file=tmpcon)
    err <- RweaveEvalWithOpt(ce, list(eval=TRUE,print=FALSE,term=TRUE,visible=FALSE))
    cat("\n") # make sure final line is complete
    sink()
    theOutput <- readLines(tmpcon)
    close(tmpcon)
    ## delete empty output
    if(length(theOutput)==1 & theOutput[1]=="") theOutput <- NULL
    
    if(inherits(err, "try-error")) {
      addToOutput(err,"\n")
    } else {
      if(!is.null(theOutput)) {
        addToOutput(paste(theOutput,sep="",collapse="\n"))
      }
    }
  }

  return(list(output = output, error=FALSE))
}


##' return all previous, or just the index most recent
setMethod(".svalue",
          signature(toolkit="ANY",obj="gCommandlineANY"),
          function(obj, toolkit, index=NULL, drop=NULL, ...) {
            theArgs = list(...);
            
            commandHistory = tag(obj,"history")
            if(length(commandHistory) == 0)
              return(c())
            if(is.null(index)) {
              return(commandHistory)
            } else {
              n = length(commandHistory)
              m = max(1, n - index + 1)
              return(rev(commandHistory[m:n]))
            }
          })

## evaluate command, store in history, swqp out widgets
setReplaceMethod(".svalue",
                 signature(toolkit="ANY",obj="gCommandlineANY"),
                 function(obj, toolkit, index=NULL, ..., value) {
                   ## get commane
                   command = value;
                   assignto = names(value)
                   if(!is.null(assignto)) {
                     command = addAssignto(command, assignto)
                   }
                   if(obj@useGUI)
                     svalue(obj@editArea,font.attr = "monospace") <-  command 

                   ## add to history
                   tag(obj, "history", replace=FALSE) <- command

                   retVal = evalChunkReturnOutput(command)

                   if(! retVal$error) {
                     .history = tag(obj,"history")
                     .history <- c(.history, command)
                     if(length(.history) > 25) .history <- .history[2:26]
                     tag(obj,"history") <- .history
                   }

                   if(obj@useGUI)
                     add(obj@outputArea, retVal$output)

                   if(obj@useConsole) {
                     cat(retVal$output)
                   }
                   
                   return(obj)
                 })

## history function
setMethod("[",
          signature(x="gCommandlineANY"),
          function(x, i, j, ..., drop=TRUE) {
            .leftBracket(x, x@toolkit, i, j, ..., drop=drop)
          })

setMethod(".leftBracket",
          signature(toolkit="ANY",x="gCommandlineANY"),
          function(x, toolkit, i, j, ..., drop=TRUE) {
            history = tag(x, "history")

            if(missing(i))
              return(history)
            else
              history(i)
          })

### working functions


## parse command(s) and make assingment on last one.
addAssignto = function(command,assignto) {
  assignto = make.names(assignto)
  tmp = unlist(strsplit(command, ";"))
  if(length(tmp)>1) {
    command = paste(tmp[-length(tmp)], Paste(assignto,"<-",tmp[length(tmp)]), collapse=";", sep=";")
  } else {
    command =  Paste(assignto,"<-", command)
  }
  return(command)
}


