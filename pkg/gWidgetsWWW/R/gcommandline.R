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


##' A Commandline for gWidgetsWWW
##'
##' @param container a container object to place commandline into
##' @param width width of group holding commandline notebook
##' @param graphic_size Size of svg objects
##' @detail This widget provides a notebook like interface for running
##' R commands for the local server. Graphics -- and only one graphic
##' per cell -- are displayed through a gsvg widget, so the \pkg{RSVGTipsDevice} must
##' be installed.
##' @note This widget implements no gWidgets methods
gcommandline <- function(container, width=NULL, graphic_size=c(480,480), ...)  {
  
  if(!gWidgetsWWWIsLocal()) {
    glabel("gcommandline can only be run locally.", container=container)
    return()
  }
  

  ##' code for a cell
  aCell <- proto(
                 ##' a new object
                 new=function(., parent, container, n) {
                   .$id <- n
                   .$parent <- parent
                   .$container <- container
                   .$g <- gexpandgroup(sprintf("[%s]", n), container=container, horizontal=FALSE)
                   .$cmdBox <- gtext("", height=16*4, width=8*80, container=.$g) # 8 pixels * 80 characters
                   .$evalButton <- gbutton("Evaluate", container=.$g)
                   .$output <- ghtml("", container=.$g) # output holder
                   .$graphics <- ggroup(container=.$g)  # graphics holder
                   
                   focus(.$cmdBox) <- TRUE
                   ## handlers: keyboard, eval button, ...
                   addHandlerClicked(.$evalButton, handler=function(h,...) {
                     . <- h$action
                     .$evalCmdLine()
                   }, action=.)
                   addHandlerKeystroke(.$cmdBox, key="e.ENTER", handler=function(h,...) {
                     . <- h$action
                     chunk <- svalue(.$cmdBox)
                     chunk <- paste(chunk, collapse="\n")
                     chunkexps <- try(parse(text=chunk), silent=TRUE)
                     if(!inherits(chunkexps, "try-error")) {
                       .$evalCmdLine()
                     }
                   },
                                       action=.)

                 },
                 ##' remove from parent container
                 remove=function(.) delete(.$container, .$g),
                 ##' how many lines to show
                 maxLines = 25,
                 ##' evaluate command. Place output into cell, do graphics f there
                 evalCmdLine=function(.) {
                   
                   ## set device and directory for graphics
                   ## requires  RSVGTipsDevice
                   curDevice <- getOption("device")
                   options(device=svg)
                   
                   curDirectory <- getwd()
                   curdir <- tempdir()
                   setwd(curdir)
                   
                   on.exit({
                     options(Device=curDevice)
                     setwd(curDirectory)
                   })
                   
                   ## check to see if we have new devices
                   noDevs <- length(dev.list())
                   
                   sapply(list.files(pattern="Rplot*"), unlink)
                   
                   ## parse to check for errors
                   ## eval and check for errors XXX
                   chunk <- svalue(.$cmdBox)
                   chunk <- paste(chunk, collapse="\n")
                   ## in Opera a mysterious %0D is added, need to delete
                   chunk <- gsub("%0D", "", chunk, fixed=TRUE)

                   
                   chunkexps <- try(parse(text=chunk), silent=TRUE)
                   if(inherits(chunkexps, "try-error")) {
                     out <- try(capture.output(eval(parse(text=chunk), envir=.GlobalEnv)), silent=TRUE)
                     if(inherits(out, "try-error"))
                       out <- sprintf("Houston, we have a problem parsing:<br>%s",
                                      chunkexps)
                   } else if(length(chunkexps) == 0) {
                     out <- ""
                   } else {
                     out <- character()
                     for(i in chunkexps) {
                       tmp <- try(capture.output(eval(i, envir=.GlobalEnv)), silent=TRUE)
                       if(inherits(tmp, "try-error")) {
                         out <- c(out, "error")
                       } else{
                         if(length(tmp))
                           out <- c(out, paste(tmp, collapse="<br>"))
                       }
                     }
                     out <- paste(out, collapse="<hr>")
                   }
                   
                   if(length(out) > .$maxLines)
                     out <- c(out[1:.$maxLines], gettext("... 8< snip >% ..."))
                   out <- paste(out, collapse="<br>")
                   out <- gsub("\\s","&nbsp;", out)
                   
                   svalue(.$output) <- sprintf("<code>%s</code>",out)
                   
                   ## handle graphics
                   if((noDevs1 <- length(dev.list())) > noDevs) {
                     for(i in (noDevs + 1):noDevs1) {
                       dev.off()
                     }
                     k <- list.files(pattern="Rplot*", path=curdir, full.names=TRUE)
                     if(length(k) > 0) {
                       if(!exists("ge",envir=.) || is.null(.$ge)) {
                         .$ge <- gexpandgroup(gettext("Plot"), container=.$graphics)
                         .$canvas <- gsvg(container=.$ge, label="fred", width=graphic_size[1], height=graphic_size[2])
                       } 
                         
                       visible(.$ge) <- TRUE
                       ## we hard code url for now -- abstract this for local
                       svalue(.$canvas) <- sprintf("http://127.0.0.1:%s/custom/%s/gWidgetsWWWRun/%s",
                                                   tools:::httpdPort, url_base,
                                                   ourURLencode(k[1]) # escapes "+"
                                                   )
                     }
                   } else {
                     ## we don't have graphics
                     if(exists("ge",envir=.)) {
                       delete(.$graphics,.$ge) # just remove
                       .$ge <- NULL
                     }
                   }
                   
                   ## make a new cell
                   if(.$parent$no_cells() <= .$parent$get_cell_index(.))
                     .$parent$new_cell()
                   
                   
                 })
  
  NotebookCells <- proto(
                         ##' list containing cells
                         cells=list(),
                         add_cell=function(., cell) {
                           tmp <- .$cells
                           tmp <- c(tmp, cell)
                           .$cells <- tmp
                         },
                         init=function(., container, width=width) {
                           .$container <- ggroup(container=container, width=width, expand=TRUE, spacing=0)
                           ## b <- gbutton("New cell", container=.$container, handler=function(h,...) {
                           ##   . <- h$action
                           ##   .$new_cell()
                           ## }, action=.)
                         },
                         ##' make new cell
                         new_cell=function(., i) {
                           if(missing(i))
                             i <- length(.$cells) + 1
                           newCell <- aCell$proto()
                           newCell$new(., .$container, i)
                           .$add_cell(newCell)
                         },
                         ##' return cell by index
                         get_cell = function(., i) {
                           if(is.numeric(i)) {
                             .$cells[[i]]
                           } else {
                             ## i is a cell object
                             ind <- sapply(.$cells, function(j) j$identical(i))
                             if(length(ind))
                               .$cells[[which(ind)]]
                             else
                               NULL
                           }
                         },
                         get_cell_index =function(., cell) {
                           ind <- sapply(.$cells, function(j) {
                             j$identical(cell)
                           })
                           which(ind)
                         },
                         ##' remove cell by index
                         remove_cell=function(., i) {
                           cell <- .$get_cell(i)
                           cell$remove()
                           l <- .$cells
                           l[[i]] <- NULL
                           .$cells <- l
                         },
                         ## move to cell by index
                         move_to_cell=function(., i) {
                           cell <- .$get_cells(i)
                           focus(cell$cmdBox) <- TRUE
                         },
                         ## how many cells
                         no_cells=function(.) length(.$cells)
                         )
  
  
  nb <- NotebookCells$proto()
  nb$init(container=container, width=width)
  nb$new_cell()

  ## return notebook. No gWidgets methods defined though
  nb
}
