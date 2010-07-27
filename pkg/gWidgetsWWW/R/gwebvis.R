##' interface to the webvis package
##'
##' @param wv If present, a webvis object
##' @param handler Not implemented
##' @param action Not implemented
##' @param container A container to place graphic into
##' @export
gwebvis <- function(wv,
                    handler=NULL, action=NULL,
                    container=NULL, ...) {

  unfold.webvis <- NULL                 # quiet down check
  if(!bypassRequire("webvis"))
    return(glabel(gettext("gwebvis needs the webvis package to be installed"), cont=container))

  
  widget <- EXTComponent$new(toplevel=container$toplevel)
  
  class(widget) <- c("gWebvis",class(widget))
  widget$toplevel$do_gwebvis <- TRUE
  if(!missing(wv))
    widget$setValue(value=wv)


  
  widget$ExtConstructor <- "Ext.Panel"
  widget$ExtCfgOptions <-  function(.) {
    out <- list()
    out[['border']] <- FALSE
    out$width <- 800; out$height <- 800
    
    out[['html']] <- String() +
      sprintf('"<div id=\'gWidgetsWebvis_%s\'><scr" + "ipt type=\'text/javascript+protovis\'></scr" + "ipt></div>"', .$ID)

    return(out)
  }
  ##' for initial graphic
  widget$footer <- function(.) {
    out <- String() +
      .$setValueJS() 
    return(out)
  }
  ##' produce javascript
  widget$setValueJS <- function(.,...) {
    out <- ""

    if(exists("..data", envir=., inherits=FALSE)) {
      value <- .$..data

      if(!is.null(value) && is(value, "webvis")) {
        ## clear out
        value$render <- "vis.root.render();"
        value <- as.character(unfold.webvis(value))
        value <- paste(value, collapse=";")
        value <- gsub("\\n",";",value)
        ## call update from Element.js (html, loadScripts, callback)
        out <- String() +
          ## note hack to avoid nesting script tags
          sprintf("val=\"<scr\" + \"ipt type='text/javascript+protovis'>%s</scr\"+\"ipt>\";", value) + "\n" +
            sprintf("Ext.fly('%s').update(val,true);", .$ID) #gWidgetsWebvis_
      }
    }
    return(out)
  }
    

  ## Handler code needs to be written. This stub just ensures it isn't
  ## written out if specified.
  widget$writeHandlersJS <- function(., signal, handler=NULL) { return("")}

    ## XXX replace when handler code added
    ##   if(!is.null(handler)) 
    ##     widget$addHandlerClicked(handler, action)
    
  
  ## add after CSS, scripts defined
  container$add(widget,...)
  invisible(widget)
  
  }


