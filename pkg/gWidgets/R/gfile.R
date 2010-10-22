##' @include guiComponents.R

##' dialog for file and directory selection
##'
##' @exports
gfile <- function(
                  text = "", type = c("open", "save", "selectdir"),
                  initialfilename = NULL,
                  filter = list("All files" = list(patterns = c("*")), "R files" = list(patterns = c("*.R",          "*.Rdata")), "text files" = list(mime.types = c("text/plain"))          ),
                  multi=FALSE,
                  handler = NULL, action = NULL, ... ,
                  toolkit=guiToolkit()){
  widget =  .gfile (toolkit,
    text=text, type=type, initialfilename=initialfilename,
    filter=filter, multi=multi, handler=handler, action=action ,...
    )
}


## generic for toolkit dispatch
setGeneric( '.gfile' ,
           function(toolkit,
                    text = "", type = c("open", "save", "selectdir"),
                    initialfilename = NULL,
                    filter = list("All files" = list(patterns = c("*")), "R files" = list(patterns = c("*.R",          "*.Rdata")), "text files" = list(mime.types = c("text/plain"))
                      ),
                    handler = NULL, action = NULL, ... )
           standardGeneric( '.gfile' ))


##' class for a widget to select a file
setClass("gFilebrowse",
         contains="guiComponent",
         prototype=prototype(new("guiComponent"))
         )


##' constructor for file/directory selection widget
##'
##' @export
gfilebrowse <- function (
                         text = "Select a file...", type = "open", quote = TRUE, 
                         container = NULL, ..., toolkit = guiToolkit()) {
  widget <- .gfilebrowse (toolkit,
                          text=text, type=type, quote=quote, container=container, ...)
  obj <- new('gFilebrowse',widget=widget,toolkit=toolkit) 
  return(obj)
}

##' generic for toolkit dispatch
##' @alias gfilebrowse
setGeneric(".gfilebrowse",
           function(toolkit,
                    text = "Select a file...", type = "open", quote = TRUE, 
                    container = NULL, ...)
           standardGeneric( '.gfilebrowse' ))
