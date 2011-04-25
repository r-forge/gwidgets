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

## creates a notebook interface tohandle plots
setClass("gGraphicsNotebookQt",
         representation=representation(
           width="numeric",height="numeric"
           ),
         contains="gNotebookQt",
         prototype=prototype(new("gNotebookQt"))
         )
setMethod(".ggraphicsnotebook",
          signature(toolkit="guiWidgetsToolkitQt"),
          function(toolkit,
                   width=dpi*6, height=dpi*6,dpi=75,
                   container = NULL,
                   ...) {

            force(toolkit)

            ## ... passed onto gggroup

            newGraph <- function(...) {
              ggraphics(cont=nb,  width=width, height=height, dpi=dpi,
                        label=sprintf("device", ""))
              svalue(nb) <- length(nb)
            }

            closeCurrent <- function(...) {
              dispose(nb)
            }
            
            acts <- list(new=gaction("New device", handler=newGraph),
                         close=gaction("Close device", handler=closeCurrent)
                         )

            g <- ggroup(cont = container, horizontal=FALSE, ...)
            bg <- ggroup(cont=g)
            lapply(acts, function(i) gbutton(action=i, cont=bg, expand=FALSE, anchor=c(-1,0)))
            addSpring(bg)
            
            nb <- gnotebook(cont=g, expand=TRUE)
            ## raise device on change
            addHandlerChanged(nb, function(h,...) {
              curDevice <- nb[h$pageno]
              if(!is.null(curDevice))
                visible(curDevice) <- TRUE
            })
            newGraph()
            
            obj <- new("gGraphicsNotebookQt",
                       block=g, widget=nb,
                       toolkit=toolkit,
                       e=new.env(), ID=getNewID()
                       )

                         
            
            
            return(obj)
            
          })
          
